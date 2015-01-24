#lang plai

(require "miraj.rkt")
(require "miraj_parser.rkt")
(require "miraj_serialization.rkt")

;;
;; Miraj interpreter
;;

(define-type Value
  (numV (n number?))
  (boolV (b boolean?))
  (symbolV (s symbol?))
  (closV (params (listof symbol?)) (body ExprC?) (env Env?))
  (boxV (l Location?))
  (voidV)
  (taggedV (tag Value?) (value Value?))
  (traceValueV (v Value?))
  (resumeV (label string?) (pos number?)))

(define-type Binding
  [bind (name symbol?) (value Value?)])
(define Location? number?)
(define Env? (listof Binding?))
(define mt-env empty)

(define-type Storage
  [cell (location Location?) (val Value?)])
(define-type Store 
  [store (cells (listof Storage?))])

(define-type Result
  [v*s*t*t (v Value?) (s Store?) (tin TraceIn?) (tout TraceOut?)])

(define-type Advice
  [aroundappsA (advice Value?)])
(define AdvStack? (listof Advice?))  
(define mt-adv empty)

(define-type Control
  [interp-init]
  [app-call (abs Value?) (args (listof Value?))]
  [app-result (r Value?)])

(define-type State
  [state (c Control?) (adv AdvStack?) (sto Store?)])

(define-type TraceIn
  [tracein (states (listof State?))])
(define mt-tracein (tracein empty))


(define-type TraceOut
  [traceout (states (listof State?))])
(define mt-traceout (traceout empty))
(define append-traceout (lambda ts
  (traceout (foldl append '() (map traceout-states ts)))))

;; Numbers and arithmetic

(define (num+ l r)
  (cond
    [(and (numV? l) (numV? r))
     (numV (+ (numV-n l) (numV-n r)))]
    [else
     (error 'num+ "one argument was not a number")]))

(define (num* l r)
  (cond
    [(and (numV? l) (numV? r))
     (numV (* (numV-n l) (numV-n r)))]
    [else
     (error 'num* "one argument was not a number")]))

(define (numWrite v)
   (cond
    [(numV? v)
     (write (numV-n v))]
    [else
     (error 'numWrite "argument was not a number")]))

;; Booleans and conditionals

(define/contract (deep-untag v) (-> Value? Value?)
  (type-case Value v
    [taggedV (tag tagged)
             (deep-untag tagged)]
    [else v]))

(define/contract (equal-values l r) (-> Value? Value? boolean?)
  (equal? l r))
     
;; Identifiers and functions

(define/contract (lookup for env) (-> symbol? Env? Value?)
  (cond
    [(empty? env) (error 'lookup (string-append "name not found: " (symbol->string for)))]
    [else 
     (type-case Binding (first env)
       [bind (name value)
             (cond
               [(symbol=? for name) value]
               [else (lookup for (rest env))])])]))

(define/contract (apply-without-weaving f args adv sto tin) (-> Value? (listof Value?) AdvStack? Store? TraceIn? Result?)
  (type-case Value (deep-untag f)
    [closV (params body env)
           (interp body (append (map bind params args) env) adv sto tin)]
    [resumeV (label pos)
             (if (unbox retroactive-error-checking)
                 (rw-call pos args adv sto)
                 (rw-call-no-error args adv sto))]
    [else (error 'interp (string-append "only functions can be applied: " (value->string f)))]))

(define z-combinator
  (parse-string "(lambda (f) ((lambda (x) (f (lambda (y) ((x x) y))))
                              (lambda (x) (f (lambda (y) ((x x) y))))))"))

;; Mutations and side-effects

; (listof Storage) Location -> Storage or #f
(define/contract (storage-at storage loc) (-> (listof Storage?) Location? any/c)
  (cond
    [(empty? storage) #f]
    [else 
     (let ([s (first storage)])
       (type-case Storage s
         [cell (l val) 
               (cond
                 [(= loc l) s]
                 [else (storage-at (rest storage) loc)])]))]))

(define/contract (fetch sto box) (-> Store? Value? Value?)
  (type-case Value box
    [boxV (loc)
          (type-case Store sto
            [store (cells)
                   (let ([storage (storage-at cells loc)])
                     (if storage
                         (type-case Storage storage
                           [cell (l val) val])
                         (error 'fetch "location not found")))])]
    [traceValueV (v) (fetch (trace-store sto) v)]
    [else (error 'interp "attempt to unbox a non-box")]))

(define/contract (trace-state t) (-> TraceIn? State?)
  (first (tracein-states t)))

(define/contract (trace-store s) (-> TraceIn? Store?)
  (state-sto (trace-state s)))

(define/contract (new-loc sto) (-> Store? Location?)
  (type-case Store sto
    [store (cells)
           (length cells)]))

(define/contract (override-store sto loc value) (-> Store? Location? Value? Store?)
  (type-case Store sto
    [store (cells)
           (store (cons (cell loc value) cells))]))

(define mt-store (store empty))

;; Mapping trace values

(define/contract (map-binding b c) (-> Binding? Value? Value?)
  (type-case Binding b
    [bind (name value)
          (type-case Value c
            [closV (params body env)
                   (closV params body (cons (bind name (map-trace-value value)) env))]
            [else (error 'map-binding "result parametmer must wrap a closure")])]))

(define/contract (map-closure params body env) (-> (listof symbol?) ExprC? Env? Value?)
  (foldr map-binding (closV params body mt-env) env))
                  
(define/contract (map-trace-value v) (-> Value? Value?)
  (type-case Value v
    [numV (_) v]
    [boolV (_) v]
    [symbolV (_) v]
    [closV (params body env)
           (map-closure params body env)]
    [boxV (l) (traceValueV v)]
    [traceValueV (tv) (traceValueV v)]
    [voidV () v]
    [taggedV (tag tagged) 
             (taggedV (map-trace-value tag) (map-trace-value tagged))]
    [resumeV (label pos) v]))

(define/contract (prepend-trace t r) (-> TraceOut? Result? Result?)
  (type-case Result r
    [v*s*t*t (v-r s-r tin-r tout-r)
             (v*s*t*t v-r s-r tin-r (append-traceout t tout-r))]))

;; Advice

; Wraps f with a single advice function
(define/contract (weave-advice adv tag advice accum) (-> AdvStack? Value? Advice? Result? Result?)
  (type-case Result accum
    [v*s*t*t (f sto tin tout)
             (prepend-trace tout (apply-without-weaving (aroundappsA-advice advice) (list tag f) adv sto tin))]))

; Wraps f according to all of the advice currently in scope.
(define/contract (weave adv f sto tin) (-> AdvStack? Value? Store? TraceIn? Result?)
  (type-case Value f
    [taggedV (tag tagged)
             (let ([woven-tagged-result (weave adv tagged sto tin)]
                   [helper (lambda (advice accum) 
                             (weave-advice adv tag advice accum))])
               (foldr helper woven-tagged-result adv))]
    [else (v*s*t*t f sto tin mt-traceout)]))

(define/contract (apply-with-weaving f args adv sto tin) (-> Value? (listof Value?) AdvStack? Store? TraceIn? Result?)
  (type-case Result (weave adv f sto tin)
      (v*s*t*t (woven-f s-w tin-w tout-w)
               (type-case Result (apply-without-weaving woven-f args adv s-w tin-w)
                 (v*s*t*t (r s-r tin-r tout-r)
                          (let ([call-state (state (app-call f args) adv sto)]
                                [return-state (state (app-result r) adv s-r)])
                            (v*s*t*t r s-r tin-r (append-traceout (traceout (list call-state)) 
                                                                  tout-w 
                                                                  tout-r 
                                                                  (traceout (list return-state))))))))))

;; Debugging

(define/contract (display-value v out) (-> Value? output-port? void?)
  (type-case Value v
    [numV (n) (display n out)]
    [boolV (b) (display b out)]
    [symbolV (s) (write s out)]
    [closV (params body env)
           (begin (display params out) (display " -> " out) (display (exp-syntax body) out) (newline out) (display-env env out))]
    [boxV (l)
          (begin (display "box(" out) (display l out) (display ")" out))]
    [traceValueV (v)
          (begin (display "tracevalue(" out) (display v out) (display ")" out))]
    [voidV ()
           (display "(void)" out)]
    [taggedV (t v)
            (begin (display "(tag " out) (display-value t out) (display " " out) (display-value v out) (display ")" out))]
    [resumeV (label f) (display label out)]))

(define/contract (value->string v) (-> Value? string?)
  (letrec ([out (open-output-string)]
           [_ (display-value v out)])
    (get-output-string out)))

(define/contract (display-context env sto t out) (-> Env? Store? TraceIn? output-port? void?)
  (begin (display "=======================================\n" out)
         (display "Environment: \n" out)
         (display-env env out)
         (display "Store: \n" out)
         (display-store sto out)
         (if (empty? t)
             (void)
             (begin (display "Trace Store: \n" out)
                    (display-store (trace-store t) out)))))
         
(define/contract (display-env env out) (-> Env? output-port? void?)
  (map (lambda (def) 
         (type-case Binding def
           [bind (n v)
                 (begin (display "\t\t" out) (display n out) (display " -> " out) (display-value v out) (display "\n" out))])) 
       env))

(define/contract (display-store sto out) (-> Store? output-port? void?)
  (map (lambda (c) 
         (type-case Storage c
           [cell (l v)
                 (begin (display "\t" out) (display l out) (display " -> " out) (display-value v out) (display "\n" out))])) 
       (store-cells sto)))

(define/contract (display-state s out) (-> State? output-port? void?)
  (type-case State s
    [state (c adv sto)
           (type-case Control c
             [interp-init ()
                       (begin (display "(interp-init)" out))]
             [app-call (f args)
                       (begin (display "(app-call " out) (display-value f out) (display ")" out))]
             [app-result (result)
                         (begin (display "(app-return " out) (display-value result out) (display ")" out))])]))
    

(define/contract (display-with-label label val out) (-> string? Value? output-port? void?)
  (begin (display label out) (display ": " out) (display-value val out) (newline out)))

;; Main interpretation function

(define verbose-interp (box false))
(define retroactive-error-checking (box true))

(define/contract (interp expr env adv sto tin) (-> ExprC? Env? AdvStack? Store? TraceIn? Result?)
(begin 
  (if (unbox verbose-interp)
      (begin
        (display "Expression: ") (display (exp-syntax expr)) (newline)
        (display-context env sto (current-output-port)) 
        (newline))
      '())

  (type-case ExprC expr
    
    ;; Numbers and arithmetic
    
    [numC (n) (v*s*t*t (numV n) sto tin mt-traceout)]
    
    [plusC (l r) (type-case Result (interp l env adv sto tin)
               [v*s*t*t (v-l s-l tin-l tout-l)
                        (type-case Result (interp r env adv s-l tin-l)
                          [v*s*t*t (v-r s-r tin-r tout-r)
                                   (v*s*t*t (num+ v-l v-r) s-r tin-r (append-traceout tout-l tout-r))])])]

    [multC (l r) (type-case Result (interp l env adv sto tin)
               [v*s*t*t (v-l s-l tin-l tout-l)
                        (type-case Result (interp r env adv s-l tin-l)
                          [v*s*t*t (v-r s-r tin-r tout-r)
                                   (v*s*t*t (num* v-l v-r) s-r tin-r (append-traceout tout-l tout-r))])])]
    
    ;; Booleans and conditionals
    
    [boolC (b) (v*s*t*t (boolV b) sto tin mt-traceout)]
    
    [equalC (l r) (type-case Result (interp l env adv sto tin)
                    [v*s*t*t (v-l s-l tin-l tout-l)
                             (type-case Result (interp r env adv s-l tin-l)
                               [v*s*t*t (v-r s-r tin-r tout-r)
                                        (v*s*t*t (boolV (equal-values v-l v-r)) s-r tin-r (append-traceout tout-l tout-r))])])]
    
    [ifC (c t f) (type-case Result (interp c env adv sto tin)
                   [v*s*t*t (v-c s-c tin-c tout-c)
                            (type-case Result (if (boolV-b v-c)
                                                  (interp t env adv s-c tin-c)
                                                  (interp f env adv s-c tin-c))
                              [v*s*t*t (v-b s-b tin-b tout-b) 
                                       (v*s*t*t v-b s-b tin-b (append-traceout tout-c tout-b))])])]
    
    ;; Identifiers and abstractions
    
    [idC (n) (v*s*t*t (lookup n env) sto tin mt-traceout)]
    
    [lamC (params b) (v*s*t*t (closV params b env) sto tin mt-traceout)]
    
    [appC (f args) (type-case Result (interp f env adv sto tin)
                  [v*s*t*t (v-f s-f tin-f tout-f)
                           (type-case ResultList (map-expr-list (lambda (e s t) (interp e env adv s t)) args s-f tin-f)
                             [vs*s*t*t (v-args s-args tin-args tout-args)
                                       (prepend-trace (append-traceout tout-f tout-args) 
                                                      (apply-with-weaving v-f v-args adv s-args tin-args))])])]
    
    [recC (f) (interp (appC z-combinator (list f)) env adv sto tin)]
    
    [letC (s v in) (type-case Result (interp v env adv sto tin)
                            [v*s*t*t (v-v s-v tin-v tout-v)
                                 (prepend-trace tout-v (interp in (cons (bind s v-v) env) adv s-v tin-v))])]
    
    ;; Boxes and sequencing
    
    [boxC (a) (type-case Result (interp a env adv sto tin)
                [v*s*t*t (v-a s-a tin-a tout-a)
                         (let ([where (new-loc sto)])
                           (v*s*t*t (boxV where)
                                    (override-store s-a where v-a)
                                    tin-a
                                    tout-a))])]
    
    [unboxC (a) (type-case Result (interp a env adv sto tin)
                  [v*s*t*t (v-a s-a tin-a tout-a)
                           (v*s*t*t (fetch s-a v-a) s-a tin-a tout-a)])]
    
    [setboxC (b val) (type-case Result (interp b env adv sto tin)
                       [v*s*t*t (v-b s-b tin-b tout-b)
                                (type-case Result (interp val env adv s-b tin-b)
                                  [v*s*t*t (v-v s-v tin-v tout-v)
                                           (type-case Value (deep-untag v-b)
                                             [boxV (l) (v*s*t*t v-v (override-store s-v l v-v) tin-v (append-traceout tout-b tout-v))]
                                             [traceValueV (v) (error 'retroactive-side-effect "attempt to retroactively set box")]
                                             [else (error 'interp "attempt to set-box! on a non-box")])])])]
    
    [seqC (b1 b2) (type-case Result (interp b1 env adv sto tin)
               [v*s*t*t (v-b1 s-b1 tin-b1 tout-b1)
                        (prepend-trace tout-b1 (interp b2 env adv s-b1 tin-b1))])]
    
    [voidC () (v*s*t*t (voidV) sto tin mt-traceout)]
    
    ;; Advice
    
    [symbolC (s) (v*s*t*t (symbolV s) sto tin mt-traceout)]
    
    [tagC (tag v) 
          (type-case Result (interp tag env adv sto tin)
            [v*s*t*t (v-tag s-tag tin-tag tout-tag)
                     (type-case Result (interp v env adv s-tag tin-tag)
                       [v*s*t*t (v-v s-v tin-v tout-v)
                                (v*s*t*t (taggedV v-tag v-v) s-v tin-v (append-traceout tout-tag tout-v))])])]
    
    [aroundappsC (wrapper extent) 
                 (type-case Result (interp wrapper env adv sto tin)
                   [v*s*t*t (v-w s-w tin-w tout-w)
                            (prepend-trace tout-w (interp extent env (cons (aroundappsA v-w) adv) s-w tin-w))])]
    
    ;; Input/Output
    
    [fileC (path) (interp (parse-file path) mt-env adv sto tin)]
    
    [writeC (l a) (type-case Result (interp a env adv sto tin)
               [v*s*t*t (v-a s-a tin-a tout-a) 
                        (begin ((unbox write-sink) (string-append l ": " (value->string v-a)))
                               (v*s*t*t v-a s-a tin-a tout-a))])]
    
    [readC (l) (let* ([val ((unbox read-source) l)]
                      [_ (record-interp-input val)])
                 (v*s*t*t (numV val) sto tin mt-traceout))]))
)
   
(define-type ResultList
  [vs*s*t*t (vs (listof Value?)) (s Store?) (tin TraceIn?) (tout TraceOut?)]) 
(define/contract (append-result rl r) (-> ResultList? Result? ResultList?)
  (type-case ResultList rl
    (vs*s*t*t (vs old-s old-tin old-tout)
              (type-case Result r
                (v*s*t*t (v s tin tout)
                         (vs*s*t*t (append vs (list v)) s tin (append-traceout old-tout tout)))))))

(define/contract (map-expr-list f exprs sto tin) (-> (-> ExprC? Store? TraceIn? Result?) (listof ExprC?) Store? TraceIn? ResultList?)
  (let ([helper (lambda (e rl) 
                  (type-case ResultList rl
                    [vs*s*t*t (vs s tin-rl tout-rl)
                              (append-result rl (f e s tin-rl))]))])
    (foldl helper (vs*s*t*t '() sto tin mt-traceout) exprs)))

(define/contract (interp-exp exp) (-> ExprC? Value?)
  (v*s*t*t-v (interp exp mt-env mt-adv mt-store mt-tracein)))
   
(define/contract (app-chain exps) (-> (listof ExprC?) ExprC?)
  (foldl (lambda (next chained) (appC chained next)) (first exps) (rest exps)))

;; Replay

(define-type MirajRecording
  [mirajRecForReplay (program list?) (input list?)])

(define/contract (interp-with-recording exps recording-path) (-> (listof ExprC?) path-string? Result?)
  (let* ([result (interp-exp (app-chain exps))]
         [input (get-interp-input)]
         [recording (mirajRecForReplay exps input)]
         [_ (write-struct-to-file recording recording-path)])
    result))
           
(define/contract (replay-interp recording-path) (-> path-string? Result?)
  (let* ([recording (read-struct-from-file recording-path)]
         [remaining-input (box (mirajRecForReplay-input recording))]
         [_ (set-box! read-source (lambda (prompt) (list-box-pop! remaining-input)))])
    ((interp-exp (app-chain (mirajRecForReplay-program recording))))))
         
;; Tracing

(define/contract (interp-with-tracing exprs trace-path) (-> (listof ExprC?) path-string? Value?)
  (type-case Result (interp (app-chain exprs) mt-env mt-adv mt-store mt-tracein)
    [v*s*t*t (v s tin tout)
           (let* ([trace (append-traceout (list (state (interp-init) mt-adv mt-store)) tout (list (state (app-result v) mt-adv s)))]
                  [_ (write-struct-to-file trace trace-path)])
             v)]))

;; TODO-RS: Gah, can't figure out how to get a hold of the current module
(define miraj-ns (module->namespace (string->path "/Users/robinsalkeld/Documents/UBC/Code/Miraj/miraj_interpreter_retroactive.rkt")))

(define/contract (interp-query trace-path exprs) (-> path-string? (listof ExprC?) Value?)
  (let ([tin (tracein (read-struct-from-file miraj-ns trace-path))])
    (if (unbox retroactive-error-checking)
        (let* ([_ (set-box! read-source (lambda (prompt) (error 'retroactive-side-effect "attempt to retroactively read input")))]
               [sto (map-trace-state (store empty))]
               [query-result (interp (app-chain exprs) mt-env mt-adv sto tin)]
               [app-state (trace-state (v*s*t*t-s query-result))]
               [weave-closure (resumeV "top-level thunk" (length (tracein-states tin)))]
               [x (apply-with-weaving (v*s*t*t-v query-result) (list weave-closure) mt-adv (v*s*t*t-s query-result))]
               [result (apply-with-weaving (v*s*t*t-v x) '() mt-adv (v*s*t*t-s x))])
          (v*s*t*t-v result))
        (let* ([sto (map-trace-state-no-error (store empty))]
               [query-result (interp (app-chain exprs) mt-env mt-adv sto mt-tracein)]
               [app-state (trace-state (v*s*t*t-s query-result))]
               [weave-closure (resumeV "dummy" 0)]
               [x (apply-with-weaving (v*s*t*t-v query-result) (list weave-closure) mt-adv (v*s*t*t-s query-result))]
               [result (apply-with-weaving (v*s*t*t-v x) '() mt-adv (v*s*t*t-s x))])
          (v*s*t*t-v result)))))


(define/contract (all-tags v) (-> Value? (listof Value?))
  (type-case Value v
    [taggedV (tag tagged)
             (cons tag (all-tags tagged))]
    [else empty]))

(define (deep-tag tags v) (-> (listof Value?) Value? Value?)
  (foldr taggedV v tags))

;; TODO-RS: Merge advice in scope properly wherever retroactive execution meets prior state

(define/contract (next-trace-state trace) (-> TraceIn? TraceIn?)
  (rest trace))

;; Without error checking

(define/contract (map-trace-state-no-error trace sto) (-> TraceIn? Store? TraceIn?)
  (type-case State (map-state-no-error (first trace) sto)
    [state (c adv mapped-sto)
           (store (store-cells mapped-sto) (cons (state c adv (state-sto (first trace))) (rest trace)))]))

(define/contract (map-state-no-error s sto) (-> State? Store? State?)
  (type-case State s
    [state (c adv t-sto)
           (type-case Control c
             [interp-init () s]
             [app-call (abs args) 
                       (state (app-call (rw-resume-value-no-error abs) (map map-trace-value args) adv sto))]
             [app-result (r)
                         (state (app-result (map-trace-value r)) adv sto)])]))

(define/contract (rw-resume-value-no-error v) (-> Value? Value?)
  (deep-tag (all-tags v) (resumeV "dummy" 0)))

(define/contract (rw-replay-call-no-error abs args adv sto) (-> Value? (listof Value?) AdvStack? Store? Result?)
  (apply-with-weaving abs args adv sto))

(define/contract (rw-call-no-error args adv sto) (-> (listof Value?) AdvStack? Store? Result?)
  (rw-result-no-error adv (map-trace-state-no-error (next-trace-state sto))))

(define/contract (rw-result-no-error adv sto tin) (-> AdvStack? Store? TraceIn? Result?)
  (type-case State (trace-state sto)
    [state (c t-adv t-sto)
           (type-case Control c
             [interp-init () 
                       (error 'rw-result-no-error "Unexpected state")]
             [app-call (abs args) 
                       (let ([result (rw-replay-call-no-error abs args adv sto)])
                         (rw-result-no-error adv (map-trace-state-no-error (next-trace-state (v*s*t*t-s result)))))]
             [app-result (r) 
                         (v*s*t*t r sto tin mt-traceout)])]))

;; With error checking 

(define/contract (map-trace-state trace sto) (-> TraceIn? Store? TraceIn?)
  (type-case State (map-state (first trace) sto)
    [state (c adv mapped-sto)
           (store (store-cells mapped-sto) (cons (state c adv (state-sto (first trace))) (rest trace)))]))

(define/contract (map-state s sto) (-> State? Store? State?)
  (type-case State s
    [state (c adv t-sto)
           (type-case Control c
             [interp-init () s]
             [app-call (abs args) 
                       (state (app-call (rw-resume-value abs sto) (map map-trace-value args)) adv sto)]
             [app-result (r)
                         (state (app-result (map-trace-value r)) adv sto)])]))

(define/contract (rw-replay-call abs args adv sto) (-> Value? (listof Value?) AdvStack? Store? Result?)
  (rw-check-result (apply-with-weaving abs args adv sto)))

(define/contract (rw-check-result result sto) (-> Result? Store? Result?)
  (type-case Result result
    [v*s*t*t (v-r s-r tin-r tout-r)
             (let ([r (app-result-r (state-c (trace-state s-r)))])
               (if (equal-values v-r r)
                   result
                   (error 'retroactive-side-effect 
                          (format "incorrect retroactive result: expected\n ~a but got\n ~a" r v-r))))]))
  
(define/contract (rw-resume-value v t) (-> Value? TraceIn? Value?)
  (let ([r (resumeV (value->string v) (length t))])
             (deep-tag (all-tags v) r)))

(define/contract (rw-call pos passed adv sto tin) 
                 (-> number? (listof Value?) AdvStack? Store? TraceIn? Result?)
  (type-case Store sto
    [store (cells)
           (cond [(= pos (length tin))
                  (type-case State (first tin)
                    [state (c t-adv t-sto)
                           (type-case Control c
                             [interp-init ()
                                          (rw-result adv (map-trace-state (next-trace-state sto)))]
                             [app-call (abs args)
                                       (if (andmap equal-values passed args)
                                           (rw-result adv (map-trace-state (next-trace-state sto)))
                                           (error 'retroactive-side-effect
                                                  (format "incorrect argument passed retroactively: expected\n ~a but got\n ~a" args passed)))]
                             [else (error 'rw-call "Unexpected state")])])]
                 [else (error 'retroactive-side-effect "retroactive advice proceeded out of order")])]))

(define/contract (rw-result adv sto tin) (-> AdvStack? Store? TraceIn? Result?)
  (let* ([t-state (trace-state sto)]
         [_ (if (unbox verbose-interp)
                (begin
                  (display "Weaving state: ") (display-state t-state (current-output-port)) (newline))
                '())])
    (type-case State t-state
      [state (c t-adv t-sto)
             (type-case Control c
               [interp-init () (error 'rw-result "Unexpected state")]
               [app-call (abs args) 
                         (let ([result (rw-replay-call abs args adv sto)])
                           (rw-result adv (map-trace-state (next-trace-state (v*s*t*t-s result)))))]
               [app-result (r) 
                           (v*s*t*t r sto tin mt-traceout)])])))