#lang plai

(require "rapl.rkt")
(require "rapl_parser.rkt")
(require "rapl_serialization.rkt")

;;
;; Rapl interpreter
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
(define Store? (listof Storage?))
(define mt-store empty)

(define-type Advice
  [aroundappsA (advice Value?)])
(define AdvStack? (listof Advice?))  
(define mt-adv empty)

(define-type Control
  [interp-init]
  [app-call (abs Value?) (args (listof Value?))]
  [app-result (r Value?)])

(define-type State
  [state (c Control?) (adv AdvStack?) (sto Store?) (tin TraceIn?)])

(define-type TraceOut
  [traceout (states (listof State?))])
(define mt-traceout (traceout empty))

(define/contract (append-traceout . ts) (->* () () #:rest (listof TraceOut?) TraceOut?)
  (traceout (foldl append '() (map traceout-states (reverse ts)))))

(define-type TraceIn
  [tracein (states (listof State?))])
(define mt-tracein (tracein empty))

(define/contract (trace-state tin) (-> TraceIn? State?)
  (first (tracein-states tin)))

(define/contract (next-trace-state tin) (-> TraceIn? TraceIn?)
  (tracein (rest (tracein-states tin))))

(define-type Result
  [v*s*t*t (v Value?) (s Store?) (tin TraceIn?) (tout TraceOut?)])

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

(define/contract (apply f args adv sto tin) (-> Value? (listof Value?) AdvStack? Store? TraceIn? Result?)
  (type-case Value f
    [closV (params body env)
           (let ([bs (map bind params args)])
             (interp body (append bs env) adv sto tin))]
    [resumeV (label pos)
             (if (unbox retroactive-error-checking)
                 (rw-call pos args adv sto tin)
                 (rw-call-no-error args adv sto tin))]
    [else (error (string-append "only functions can be applied: " (value->string f)))]))

(define z-combinator
  (parse-string "(lambda (f) ((lambda (x) (f (lambda (y) ((x x) y))))
                              (lambda (x) (f (lambda (y) ((x x) y))))))"))

;; Mutations and side-effects

; (listof Storage) Location -> Storage or #f
(define/contract (storage-at storage loc) (-> (listof Storage?) Location? (or/c Storage? #f))
  (cond
    [(empty? storage) #f]
    [else 
     (let ([s (first storage)])
       (type-case Storage s
         [cell (l val) 
               (cond
                 [(= loc l) s]
                 [else (storage-at (rest storage) loc)])]))]))

(define/contract (fetch sto tin b) (-> Store? TraceIn? Value? Value?)
  (type-case Value b
    [boxV (loc)
          (let ([storage (storage-at sto loc)])
            (if storage
                (type-case Storage storage
                  [cell (l val) val])
                (error "location not found")))]
    [traceValueV (v) 
                 (type-case State (trace-state tin)
                   [state (c adv sto-t tin-t)
                          (fetch sto-t tin-t v)])]
    [else (error "attempt to unbox a non-box")]))

(define/contract (new-loc sto) (-> Store? Location?)
 (length sto))

(define override-store cons)

;; Lifting trace values

(define/contract (lift-binding b c) (-> Binding? Value? Value?)
  (type-case Binding b
    [bind (name value)
          (type-case Value c
            [closV (params body env)
                   (closV params body (cons (bind name (lift-trace-value value)) env))]
            [else (error "result parametmer must wrap a closure")])]))

(define/contract (lift-closure params body env) (-> (listof symbol?) ExprC? Env? Value?)
  (foldr lift-binding (closV params body mt-env) env))
                  
(define/contract (lift-trace-value v) (-> Value? Value?)
  (type-case Value v
    [numV (_) v]
    [boolV (_) v]
    [symbolV (_) v]
    [closV (params body env)
           (lift-closure params body env)]
    [boxV (l) (traceValueV v)]
    [traceValueV (tv) (traceValueV v)]
    [voidV () v]
    [taggedV (tag tagged) 
             (taggedV (lift-trace-value tag) (lift-trace-value tagged))]
    [resumeV (label pos) v]))

(define/contract (prepend-trace t r) (-> TraceOut? Result? Result?)
  (type-case Result r
    [v*s*t*t (v-r s-r tin-r tout-r)
             (v*s*t*t v-r s-r tin-r (append-traceout t tout-r))]))

;; Advice

(define/contract (apply-with-weaving f args adv sto tin) (-> Value? (listof Value?) AdvStack? Store? TraceIn? Result?)
  (type-case Result (weave adv f sto tin)
      (v*s*t*t (woven-f s-w tin-w tout-w)
               (type-case Result (apply woven-f args adv s-w tin-w)
                 (v*s*t*t (r s-r tin-r tout-r)
                          (let ([call-state (state (app-call f args) adv sto tin)]
                                [return-state (state (app-result r) adv s-r tin-r)])
                            (v*s*t*t r s-r tin-r (append-traceout (traceout (list call-state)) 
                                                                  tout-w 
                                                                  tout-r 
                                                                  (traceout (list return-state))))))))))

; Applies all advice in scope for all tags on f
(define/contract (weave adv f sto tin) (-> AdvStack? Value? Store? TraceIn? Result?)
  (type-case Value f
    [taggedV (tag tagged)
             (type-case Result (weave adv tagged sto tin)
               [v*s*t*t (v-w s-w tin-w tout-w)
                        (prepend-trace tout-w (weave-for-tag adv tag v-w s-w tin-w))])]
    [else (v*s*t*t f sto tin mt-traceout)]))

; Applies all advice in scope for a single tag on f
(define/contract (weave-for-tag adv tag f sto tin) (-> AdvStack? Value? Value? Store? TraceIn? Result?)
  (if (empty? adv)
      (v*s*t*t f sto tin mt-traceout)
      (type-case Result (weave-advice adv tag (first adv) f sto tin)
        [v*s*t*t (v-w s-w tin-w tout-w)
                 (prepend-trace tout-w (weave-for-tag (rest adv) tag v-w s-w tin-w))])))

; Apply a single advice function to f
(define/contract (weave-advice adv tag advice f sto tin) (-> AdvStack? Value? Advice? Value? Store? TraceIn? Result?)
  (type-case Advice advice
    [aroundappsA (g)
                 (apply g (list tag f) adv sto tin)]))

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
         (if (empty? (tracein-states t))
             (void)
             (begin (display "Trace Store: \n" out)
                    (display-store (state-sto (trace-state t)) out)))))
         
(define/contract (display-env env out) (-> Env? output-port? void?)
  (for ([def env]) 
    (type-case Binding def
      [bind (n v)
            (begin (display "\t\t" out) (display n out) (display " -> " out) (display-value v out) (display "\n" out))])))

(define/contract (display-store sto out) (-> Store? output-port? void?)
  (for ([c sto])
    (type-case Storage c
      [cell (l v)
            (begin (display "\t" out) (display l out) (display " -> " out) (display-value v out) (display "\n" out))])))

(define/contract (display-state s out) (-> State? output-port? void?)
  (type-case State s
    [state (c adv sto tin)
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
        (display-context env sto tin (current-output-port)) 
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
                                    (override-store (cell where v-a) sto)
                                    tin-a
                                    tout-a))])]
    
    [unboxC (a) (type-case Result (interp a env adv sto tin)
                  [v*s*t*t (v-a s-a tin-a tout-a)
                           (v*s*t*t (fetch s-a tin-a v-a) s-a tin-a tout-a)])]
    
    [setboxC (b val) (type-case Result (interp b env adv sto tin)
                       [v*s*t*t (v-b s-b tin-b tout-b)
                                (type-case Result (interp val env adv s-b tin-b)
                                  [v*s*t*t (v-v s-v tin-v tout-v)
                                           (type-case Value (deep-untag v-b)
                                             [boxV (l) (v*s*t*t (voidV) 
                                                                (override-store (cell l v-v) s-v) 
                                                                tin-v 
                                                                (append-traceout tout-b tout-v))]
                                             [traceValueV (v) (error 'retroactive-side-effect "attempt to retroactively set box")]
                                             [else (error 'interp "attempt to set-box! on a non-box")])])])]
    
    [seqC (b1 b2) (type-case Result (interp b1 env adv sto tin)
               [v*s*t*t (v-b1 s-b1 tin-b1 tout-b1)
                        (prepend-trace tout-b1 (interp b2 env adv s-b1 tin-b1))])]
    
    [voidC () (v*s*t*t (voidV) sto tin mt-traceout)]
    
    ;; Advice
    
    [symbolC (s) (v*s*t*t (symbolV s) sto tin mt-traceout)]
    
    [tagC (tag v) 
          (interp-tag tag v env adv sto tin)]
    
    [aroundappsC (advice extent)  
                 (interp-aroundapps advice extent env adv sto tin)]
    
    ;; Input/Output
    
    [fileC (path) (interp (parse-file path) mt-env adv sto tin)]
    
    [writeC (l a) (type-case Result (interp a env adv sto tin)
               [v*s*t*t (v-a s-a tin-a tout-a) 
                        (begin ((unbox write-sink) (string-append l ": " (value->string v-a)))
                               (v*s*t*t (voidV) s-a tin-a tout-a))])]
    
    [readC (l) (let* ([val ((unbox read-source) l)]
                      [_ (record-interp-input val)])
                 (v*s*t*t (numV val) sto tin mt-traceout))]))
)

;; Separated these cases out to keep the code snippets narrow for the paper

(define/contract (interp-tag tag v env adv sto tin) (-> ExprC? ExprC? Env? AdvStack? Store? TraceIn? Result?)
  (type-case Result (interp tag env adv sto tin)
    [v*s*t*t (v-tag s-tag tin-tag tout-tag)
             (type-case Result (interp v env adv s-tag tin-tag)
               [v*s*t*t (v-v s-v tin-v tout-v)
                        (v*s*t*t (taggedV v-tag v-v) s-v tin-v (append-traceout tout-tag tout-v))])]))

(define/contract (interp-aroundapps advice extent env adv sto tin) (-> ExprC? ExprC? Env? AdvStack? Store? TraceIn? Result?)
  (type-case Result (interp advice env adv sto tin)
    [v*s*t*t (v-a s-a tin-a tout-a)
             (let ([new-adv (cons (aroundappsA v-a) adv)])
               (prepend-trace tout-a (interp extent env new-adv s-a tin-a)))]))

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
           (let ([trace (append (list (state (interp-init) mt-adv mt-store mt-tracein))
                                 (traceout-states tout)
                                 (list (state (app-result v) mt-adv s tin)))])
             (begin 
               (if (file-exists? trace-path)
                   (delete-file trace-path)
                   (void))
               (write-struct-to-file trace trace-path)
               v))]))

;; TODO-RS: Gah, can't figure out how to get a hold of the current module
(define miraj-ns (module->namespace (string->path "/Users/robinsalkeld/Documents/UBC/Code/rapl/rapl_interpreter.rkt")))

(define/contract (interp-query trace-path exprs) (-> path-string? (listof ExprC?) Value?)
  (let* ([_ (set-box! read-source (lambda (prompt) (error 'retroactive-side-effect "attempt to retroactively read input")))]
         [tin (tracein (read-struct-from-file miraj-ns trace-path))]
         [resume (resumeV "top-level thunk" (length (tracein-states tin)))])
    (type-case Result (interp (app-chain exprs) mt-env mt-adv mt-store mt-tracein)
      [v*s*t*t (v-a s-a tin-a tout-a)
               (type-case Result (apply-with-weaving v-a (list resume) mt-adv s-a tin)
                 [v*s*t*t (v-t s-t tin-t tout-t)
                          (type-case Result (apply v-t (list) mt-adv s-t tin-t)
                            [v*s*t*t (v-r s-r tin-r tout-r) 
                                     (if (or #t (= (length (tracein-states tin-r)) 0))
                                         v-r
                                         (error 'interp-query "Trace not fully read ~s" tin-r))])])])))

(define/contract (all-tags v) (-> Value? (listof Value?))
  (type-case Value v
    [taggedV (tag tagged)
             (cons tag (all-tags tagged))]
    [else empty]))

(define (deep-tag tags v) (-> (listof Value?) Value? Value?)
  (foldr taggedV v tags))

;; TODO-RS: Merge advice in scope properly wherever retroactive execution meets prior state

;; Without error checking

(define/contract (rw-resume-value-no-error v) (-> Value? Value?)
  (deep-tag (all-tags v) (resumeV "dummy" 0)))

(define/contract (rw-replay-call-no-error f args adv sto tin) (-> Value? (listof Value?) AdvStack? Store? TraceIn? Result?)
  (apply-with-weaving (rw-resume-value-no-error f) (map lift-trace-value args) adv sto tin))

(define/contract (rw-call-no-error args adv sto tin) (-> (listof Value?) AdvStack? Store? TraceIn? Result?)
  (rw-result-no-error adv sto (next-trace-state tin)))

(define/contract (rw-result-no-error adv sto tin) (-> AdvStack? Store? TraceIn? Result?)
  (type-case Control (state-c (trace-state tin))
    [interp-init () 
                 (error 'rw-result-no-error "Unexpected state")]
    [app-call (f args) 
              (type-case Result (rw-replay-call-no-error f args adv sto tin)
                (v*s*t*t (v-r s-r tin-r tout-r)
                         (rw-result-no-error adv s-r (next-trace-state tin-r))))]
    [app-result (r) 
                (v*s*t*t (lift-trace-value r) sto tin mt-traceout)]))

;; With error checking 

(define/contract (rw-replay-call f args adv sto tin) (-> Value? (listof Value?) AdvStack? Store? TraceIn? Result?)
  (type-case Result (rw-check-result 
                     (apply-with-weaving (rw-resume-value f tin) 
                                         (map lift-trace-value args) 
                                         adv sto tin)
                     tin)
    [v*s*t*t (v-r s-r tin-r tout-r)
             (v*s*t*t v-r s-r (next-trace-state tin-r) tout-r)]))

(define/contract (rw-check-result result tin-before) (-> Result? TraceIn? Result?)
  (type-case Result result
    [v*s*t*t (v-r s-r tin-r tout-r)
             (if (< (length (tracein-states tin-r)) (length (tracein-states tin-before)))
                 (let ([r (app-result-r (state-c (trace-state tin-r)))])
                   (if (equal-values v-r r)
                       result
                       (error 'retroactive-side-effect 
                              (format "incorrect retroactive result: expected\n ~a but got\n ~a" r v-r))))
                 (error 'retroactive-side-effect "retroactive advice did not proceed"))]))
  
(define/contract (rw-resume-value v t) (-> Value? TraceIn? Value?)
  (let ([r (resumeV (value->string v) (length (tracein-states t)))])
    (deep-tag (all-tags v) r)))

(define/contract (rw-call pos passed adv sto tin)
                 (-> number? (listof Value?) AdvStack? Store? TraceIn? Result?)
  (cond [(= pos (length (tracein-states tin)))
         (if (empty? (state-adv (trace-state tin)))
             (type-case Control (state-c (trace-state tin))
               [interp-init ()
                            (rw-result adv sto (next-trace-state tin))]
               [app-call (abs args)
                         (if (andmap equal-values passed (map lift-trace-value args))
                             (rw-result adv sto (next-trace-state tin))
                             (error 'retroactive-side-effect
                                    (format "incorrect argument passed retroactively: expected\n ~a but got\n ~a" args passed)))]
               [else (error 'rw-call "Unexpected state")])
             (error 'retroactive-side-effect "traces with advice are not supported"))]
        [else (error 'retroactive-side-effect "retroactive advice proceeded out of order")]))

(define/contract (rw-result adv sto tin) (-> AdvStack? Store? TraceIn? Result?)
  (let* ([t-state (trace-state tin)]
         [_ (if (unbox verbose-interp)
                (begin
                  (display "Weaving state: ") (display-state t-state (current-output-port)) (newline))
                '())])
    (type-case State t-state
      [state (c adv-t sto-t tin-t)
             (type-case Control c
               [interp-init () (error 'rw-result "Unexpected state")]
               [app-call (f args) 
                         (type-case Result (rw-replay-call f args adv sto tin)
                           (v*s*t*t (v-r s-r tin-r tout-r)
                                    (prepend-trace tout-r (rw-result adv s-r tin-r))))]
               [app-result (r) 
                           (v*s*t*t (lift-trace-value r) sto tin mt-traceout)])])))