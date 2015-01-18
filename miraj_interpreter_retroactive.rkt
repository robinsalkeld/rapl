#lang plai

(require "miraj.rkt")
(require "miraj_parser.rkt")
(require "miraj_serialization.rkt")

(define (check x contract)
  (if (contract x)
      x
      (raise-arguments-error 'check "contract violated"
                             "contract" contract
                             "given" x)))

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
  (resumeV (label string?) (pos number?)))

(define-type Binding
  [bind (name symbol?) (value Value?)])
(define Location? number?)
(define Env? (listof Binding?))
(define mt-env empty)

(define-type Storage
  [cell (location Location?) (val Value?)]
  [mapping (location Location?) (trace-loc Location?)])
(define-type Store 
  [store (cells (listof Storage?)) (t Trace?)])

(define-type Result
  [v*s*t (v Value?) (s Store?) (t Trace?)])

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
(define Trace? (listof State?))
(define mt-trace empty)

;; Numbers and arithmetic

(define (num+ l r)
  (cond
    [(and (numV? l) (numV? r))
     (numV (+ (numV-n l) (numV-n r)))]
    [else
     (error 'num+ "one argument was not a number")]))

(define (num* l r) Value?
  (cond
    [(and (numV? l) (numV? r))
     (numV (* (numV-n l) (numV-n r)))]
    [else
     (error 'num* "one argument was not a number")]))

(define (numWrite (v Value?))
   (cond
    [(numV? v)
     (write (numV-n v))]
    [else
     (error 'numWrite "argument was not a number")]))

;; Booleans and conditionals

(define (deep-untag [v Value?])
  (type-case Value v
    [taggedV (tag tagged)
             (deep-untag tagged)]
    [else v]))

(define (equal-values [l Value?] [r Value?]) Value?
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

(define/contract (apply-without-weaving f args adv sto) (-> Value? (listof Value?) AdvStack? Store? Result?)
  (type-case Value (deep-untag f)
    [closV (params body env)
           (interp body (append (map bind params args) env) adv (check sto Store?))]
    [resumeV (label pos)
             (if (unbox retroactive-error-checking)
                 (rw-call pos args adv sto)
                 (rw-call-no-error args adv sto))]
    [else (error 'interp (string-append "only functions can be applied: " (value->string f)))]))

(define z-combinator
  (parse-string "(lambda (f) ((lambda (x) (f (lambda (y) ((x x) y))))
                              (lambda (x) (f (lambda (y) ((x x) y))))))"))

;; Mutations and side-effects

(define (storage-at [storage (listof Storage?)] [loc Location?]) Storage?
  (cond
    [(empty? storage) #f]
    [else 
     (let ([s (first storage)])
       (type-case Storage s
         [cell (l val) 
               (cond
                 [(= loc l) s]
                 [else (storage-at (rest storage) loc)])]
         [mapping (l t-loc) 
                  (cond
                    [(= loc l) s]
                    [else (storage-at (rest storage) loc)])]))]))

(define (fetch [sto Store?] [loc Location?]) Value?
  (type-case Store sto
    [store (cells t)
           (let ([storage (storage-at cells loc)])
             (if storage
                 (type-case Storage storage
                   [cell (l val) val]
                   [mapping (l t-loc)
                            (let ([trace-value (fetch (trace-store sto) t-loc)])
                              (type-case Result (map-trace-value trace-value sto)
                                [v*s*t (v s-v t-v) v]))])
                 (error 'fetch "location not found")))]))

(define (trace-state [s Store?]) State?
  (first (store-t s)))

(define (trace-store [s Store?]) Store?
  (state-sto (trace-state s)))

(define (new-loc [sto Store?]) Location?
  (type-case Store sto
    [store (cells t)
           (length cells)]))

(define (override-store [sto Store?] [loc Location?] [value Value?]) Store?
  (type-case Store sto
    [store (cells trace)
           (if (mapping? (storage-at cells loc))
               (error 'retroactive-side-effect "attempt to retroactively set box")
               (store (cons (cell loc value) cells) trace))]))

(define mt-store (store empty empty))

;; Mapping trace values

(define (mapped-location [cells (listof Storage?)] [loc Location?]) Value?
  (cond
    [(empty? cells) #f]
    [else 
     (type-case Storage (first cells)
       [cell (l val)
             (mapped-location (rest cells) loc)]
       [mapping (l t-loc)
             (cond
               [(= loc t-loc) l]
               [else (mapped-location (rest cells) loc)])])]))

(define (map-trace-location [trace-loc Location?] [sto Store?]) Result?
  (type-case Store sto
    [store (cells t)
           (let ([mapped-loc (mapped-location cells trace-loc)])
             (if mapped-loc
                 (v*s*t (boxV mapped-loc) sto mt-trace)
                 (let* ([loc (new-loc sto)]
                        [sto2 (store (cons (mapping loc trace-loc) cells) t)]
                        [trace-value (fetch (trace-store sto2) trace-loc)]
                        [value-result (map-trace-value trace-value sto2)])
                   (v*s*t (boxV loc) (v*s*t-s value-result) mt-trace))))]))

(define (map-binding [b Binding?] [result Result?]) Result?
  (type-case Binding b
    [bind (name value)
          (type-case Result result
            [v*s*t (c sto t)
                   (type-case Value c
                     [closV (params body env)
                            (type-case Result (map-trace-value value sto)
                              [v*s*t (m s-m t-m)
                                     (v*s*t (closV params body (cons (bind name m) env)) s-m mt-trace)])]
                     [else (error 'map-binding "result parametmer must wrap a closure")])])]))

(define (map-closure [params (listof symbol?)] [body ExprC?] [env Env?] [sto Store?]) Result?
  (foldr map-binding (v*s*t (closV params body mt-env) sto mt-trace) env))
                  
(define (map-trace-value (v Value?) (sto Store?)) Result?
  (type-case Value v
    [numV (_) 
          (v*s*t v sto mt-trace)]
    [boolV (_) 
          (v*s*t v sto mt-trace)]
    [symbolV (_) 
          (v*s*t v sto mt-trace)]
    [closV (params body env)
           (map-closure params body env sto)]
    [boxV (trace-loc)
          (map-trace-location trace-loc sto)]
    [voidV ()
           (v*s*t v sto mt-trace)]
    [taggedV (tag tagged) 
             (type-case Result (map-trace-value tag sto)
               [v*s*t (mapped-tag s-tag t-tag)
                      (type-case Result (map-trace-value tagged s-tag)
                        [v*s*t (mapped-tagged s-tagged t-tagged)
                               (v*s*t (taggedV mapped-tag mapped-tagged) s-tagged mt-trace)])])]
    [resumeV (label pos) (v*s*t v sto mt-trace)]))

(define (prepend-trace [t Trace?] [r Result?]) Result?
  (type-case Result r
    [v*s*t (v-r s-r t-r)
           (v*s*t v-r s-r (append t t-r))]))

;; Advice

; Wraps f with a single advice function
(define (weave-advice [adv AdvStack?] [tag Value?] [advice Advice?] [accum Result?]) Result?
  (type-case Result accum
    [v*s*t (f sto t)
           (prepend-trace t (apply-without-weaving (aroundappsA-advice advice) (list tag f) adv (check sto Store?)))]))

; Wraps f according to all of the advice currently in scope
(define (weave [adv AdvStack?] [f Value?] [sto Store?]) Result?
  (type-case Value f
    [taggedV (tag tagged)
             (let ([woven-tagged-result (weave adv tagged sto)]
                   [helper (lambda (advice accum) 
                             (weave-advice adv tag advice accum))])
               (foldr helper woven-tagged-result adv))]
    [else (v*s*t f sto mt-trace)]))

(define (apply-with-weaving [f Value?] [args (listof Value?)] [adv AdvStack?] [sto Store?]) Result?
  (type-case Result (weave adv f (check sto Store?))
      (v*s*t (woven-f s-w t-w)
             (type-case Result (apply-without-weaving woven-f args adv (check s-w Store?))
                 (v*s*t (r s-r t-r)
                        (let ([call-state (state (app-call f args) adv sto)]
                              [return-state (state (app-result r) adv s-r)])
                            (v*s*t r s-r (append (list call-state) t-w t-r (list return-state)))))))))

;; Debugging

(define (display-value [v Value?] [out output-port?])
  (type-case Value v
    [numV (n) (display n out)]
    [boolV (b) (display b out)]
    [symbolV (s) (write s out)]
    [closV (params body env)
           (begin (display params out) (display " -> " out) (display (exp-syntax body) out) (newline out) (display-env env out))]
    [boxV (l)
          (begin (display "box(" out) (display l out) (display ")" out))]
    [voidV ()
           (display "(void)" out)]
    [taggedV (t v)
            (begin (display "(tag " out) (display-value t out) (display " " out) (display-value v out) (display ")" out))]
    [resumeV (label f) (display label out)]))

(define (value->string [v Value?]) string?
  (letrec ([out (open-output-string)]
           [_ (display-value v out)])
    (get-output-string out)))

(define (display-context [env Env?] [sto Store?] [out output-port?])
  (begin (display "=======================================\n" out)
         (display "Environment: \n" out)
         (display-env env out)
         (display "Store: \n" out)
         (display-store sto out)
         (if (empty? (store-t sto))
             (void)
             (begin (display "Trace Store: \n" out)
                    (display-store (trace-store sto) out)))))
         
(define (display-env [env Env?] [out output-port?])
  (map (lambda (def) 
         (type-case Binding def
           [bind (n v)
                 (begin (display "\t\t" out) (display n out) (display " -> " out) (display-value v out) (display "\n" out))])) 
       env))

(define (display-store [sto Store?] [out output-port?])
  (map (lambda (c) 
         (type-case Storage c
           [cell (l v)
                 (begin (display "\t" out) (display l out) (display " -> " out) (display-value v out) (display "\n" out))]
           [mapping (l t-l)
                    (begin (display "\t" out) (display l out) (display " -> [" out) (display t-l out) (display "]\n" out))])) 
       (store-cells sto)))

(define (display-state [s State?] [out output-port?])
  (type-case State s
    [state (c adv sto)
           (type-case Control c
             [interp-init ()
                       (begin (display "(interp-init)" out))]
             [app-call (f args)
                       (begin (display "(app-call " out) (display-value f out) (display ")" out))]
             [app-result (result)
                         (begin (display "(app-return " out) (display-value result out) (display ")" out))])]))
    

(define (display-with-label [label string?] [val Value?] [out output-port?])
  (begin (display label out) (display ": " out) (display-value val out) (newline out)))

;; Main interpretation function

(define verbose-interp (box false))
(define retroactive-error-checking (box true))

(define (interp [expr ExprC?] [env Env?] [adv AdvStack?] [sto Store?]) Result?
(begin 
  (if (unbox verbose-interp)
      (begin
        (check sto Store?)
        (display "Expression: ") (display (exp-syntax expr)) (newline)
        (display-context env sto (current-output-port)) 
        (newline))
      '())

  (type-case ExprC expr
    
    ;; Numbers and arithmetic
    
    [numC (n) (v*s*t (numV n) sto mt-trace)]
    
    [plusC (l r) (type-case Result (interp l env adv sto)
               [v*s*t (v-l s-l t-l)
                      (type-case Result (interp r env adv s-l)
                        [v*s*t (v-r s-r t-r)
                               (v*s*t (num+ v-l v-r) s-r (append t-l t-r))])])]

    [multC (l r) (type-case Result (interp l env adv sto)
               [v*s*t (v-l s-l t-l)
                      (type-case Result (interp r env adv s-l)
                        [v*s*t (v-r s-r t-r)
                               (v*s*t (num* v-l v-r) s-r (append t-l t-r))])])]
    
    ;; Booleans and conditionals
    
    [boolC (b) (v*s*t (boolV b) sto mt-trace)]
    
    [equalC (l r) (type-case Result (interp l env adv sto)
               [v*s*t (v-l s-l t-l)
                      (type-case Result (interp r env adv s-l)
                        [v*s*t (v-r s-r t-r)
                               (v*s*t (boolV (equal-values v-l v-r)) s-r (append t-l t-r))])])]
    
    [ifC (c t f) (type-case Result (interp c env adv sto)
                   [v*s*t (v-c s-c t-c)
                          (type-case Result (if (boolV-b v-c)
                                                (interp t env adv s-c)
                                                (interp f env adv s-c))
                            [v*s*t (v-b s-b t-b) 
                                   (v*s*t v-b s-b (append t-c t-b))])])]
    
    ;; Identifiers and abstractions
    
    [idC (n) (v*s*t (lookup n env) sto mt-trace)]
    
    [lamC (params b) (v*s*t (closV params b env) sto mt-trace)]
    
    [appC (f args) (type-case Result (interp f env adv sto)
                  [v*s*t (v-f s-f t-f)
                         (type-case ResultList (map-expr-list (lambda (e s) (interp e env adv s)) args s-f)
                           [vs*s*t (v-args s-args t-args)
                                  (type-case Result (apply-with-weaving v-f v-args adv (check s-args Store?))
                                    [v*s*t (v-r s-r t-r)
                                           (v*s*t v-r s-r (append t-f t-args t-r))])])])]
    
    [recC (f) (interp (appC z-combinator (list f)) env adv sto)]
    
    [letC (s v in) (type-case Result (interp v env adv sto)
                            [v*s*t (v-v s-v t-v)
                                 (prepend-trace t-v (interp in (cons (bind s v-v) env) adv s-v))])]
    
    ;; Boxes and sequencing
    
    [boxC (a) (type-case Result (interp a env adv sto)
                [v*s*t (v-a s-a t-a)
                       (let ([where (new-loc sto)])
                         (v*s*t (boxV where)
                                (override-store s-a where v-a)
                                t-a))])]
    
    [unboxC (a) (type-case Result (interp a env adv sto)
                  [v*s*t (v-a s-a t-a)
                         (v*s*t (fetch s-a (boxV-l (deep-untag v-a))) s-a t-a)])]
    
    [setboxC (b val) (type-case Result (interp b env adv sto)
                       [v*s*t (v-b s-b t-b)
                              (type-case Result (interp val env adv s-b)
                                [v*s*t (v-v s-v t-v)
                                       (let ([where (boxV-l (deep-untag v-b))])
                                         (v*s*t v-v (override-store s-v where v-v) (append t-b t-v)))])])]
    
    [seqC (b1 b2) (type-case Result (interp b1 env adv sto)
               [v*s*t (v-b1 s-b1 t-b1)
                      (prepend-trace t-b1 (interp b2 env adv s-b1))])]
    
    [voidC () (v*s*t (voidV) sto mt-trace)]
    
    ;; Advice
    
    [symbolC (s) (v*s*t (symbolV s) sto mt-trace)]
    
    [tagC (tag v) 
          (type-case Result (interp tag env adv sto)
            [v*s*t (v-tag s-tag t-tag)
                   (type-case Result (interp v env adv s-tag)
                     [v*s*t (v-v s-v t-v)
                            (v*s*t (taggedV v-tag v-v) s-v (append t-tag t-v))])])]
    
    [aroundappsC (wrapper extent) 
                 (type-case Result (interp wrapper env adv sto)
                   [v*s*t (v-w s-w t-w)
                          (prepend-trace t-w (interp extent env (cons (aroundappsA v-w) adv) s-w))])]
    
    ;; Input/Output
    
    [fileC (path) (interp (parse-file path) mt-env adv sto)]
    
    [writeC (l a) (type-case Result (interp a env adv sto)
               [v*s*t (v-a s-a t-a) 
                      (begin ((unbox write-sink) (string-append l ": " (value->string v-a)))
                               (v*s*t v-a s-a t-a))])]
    
    [readC (l) (let* ([val ((unbox read-source) l)]
                      [_ (record-interp-input val)])
                 (v*s*t (numV val) sto mt-trace))]))
)
   
(define-type ResultList
  [vs*s*t (vs (listof Value?)) (s Store?) (t Trace?)]) 
(define (append-result [rl ResultList?] [r Result?]) ResultList?
  (type-case ResultList rl
    (vs*s*t (vs old-s old-t)
            (type-case Result r
              (v*s*t (v s t)
                     (vs*s*t (append vs (list v)) s (append old-t t)))))))

(define (map-expr-list [f procedure?] [exprs (listof ExprC?)] [sto Store?]) ResultList?
  (let ([helper (lambda (e rl) 
                  (append-result rl (f e (vs*s*t-s rl))))])
    (foldl helper (vs*s*t '() sto mt-trace) exprs)))

(define (interp-exp [exp ExprC?]) Value?
  (v*s*t-v (interp exp mt-env mt-adv mt-store)))
   
(define (app-chain [exps list?]) ExprC?
  (foldl (lambda (next chained) (appC chained next)) (first exps) (rest exps)))

;; Replay

(define-type MirajRecording
  [mirajRecForReplay (program list?) (input list?)])

(define (interp-with-recording (exps list?) (recording-path path-string?))
  (let* ([result (interp-exp (app-chain exps))]
         [input (get-interp-input)]
         [recording (mirajRecForReplay exps input)]
         [_ (write-struct-to-file recording recording-path)])
    result))
           
(define (replay-interp (recording-path path-string?))
  (let* ([recording (read-struct-from-file recording-path)]
         [remaining-input (box (mirajRecForReplay-input recording))]
         [_ (set-box! read-source (lambda (prompt) (list-box-pop! remaining-input)))])
    ((interp-exp (app-chain (mirajRecForReplay-program recording))))))
         
;; Tracing

(define-type MirajTrace
  [mirajTrace (jps Trace?)])

(define (interp-with-tracing (exprs (listof ExprC?)) (trace-path path-string?)) Value?
  (type-case Result (interp (app-chain exprs) mt-env mt-adv mt-store)
    [v*s*t (v s t)
           (let* ([trace (append (list (state (interp-init) mt-adv mt-store)) t (list (state (app-result v) mt-adv s)))]
                  [_ (write-struct-to-file (mirajTrace trace) trace-path)])
             v)]))

;; TODO-RS: Gah, can't figure out how to get a hold of the current module
(define miraj-ns (module->namespace (string->path "/Users/robinsalkeld/Documents/UBC/Code/Miraj/miraj_interpreter_retroactive.rkt")))

(define (interp-query (trace-path path-string?) (exprs list?)) Value?
  (type-case MirajTrace (read-struct-from-file miraj-ns trace-path)
    [mirajTrace (app-trace)
                (if (unbox retroactive-error-checking)
                    (let* ([_ (set-box! read-source (lambda (prompt) (error 'retroactive-side-effect "attempt to retroactively read input")))]
                           [sto (map-trace-state (store empty app-trace))]
                           [query-result (interp (app-chain exprs) mt-env mt-adv sto)]
                           [app-state (trace-state (v*s*t-s query-result))]
                           [weave-closure (resumeV "top-level thunk" (length app-trace))]
                           [x (apply-with-weaving (v*s*t-v query-result) (list weave-closure) mt-adv (v*s*t-s query-result))]
                           [result (apply-with-weaving (v*s*t-v x) '() mt-adv (v*s*t-s x))])
                      (v*s*t-v result))
                    (let* ([sto (map-trace-state-no-error (store empty app-trace))]
                           [query-result (interp (app-chain exprs) mt-env mt-adv sto)]
                           [app-state (trace-state (v*s*t-s query-result))]
                           [weave-closure (resumeV "dummy" 0)]
                           [x (apply-with-weaving (v*s*t-v query-result) (list weave-closure) mt-adv (v*s*t-s query-result))]
                           [result (apply-with-weaving (v*s*t-v x) '() mt-adv (v*s*t-s x))])
                      (v*s*t-v result)))]))


(define (all-tags [v Value?]) (listof Value?)
  (type-case Value v
    [taggedV (tag tagged)
             (cons tag (all-tags tagged))]
    [else empty]))

(define (deep-tag [tags (listof Value?)] [v Value?]) Value?
  (foldr taggedV v tags))

;; TODO-RS: Merge advice in scope properly wherever retroactive execution meets prior state

(define (next-trace-state [s Store?]) Store?
  (type-case Store s
    [store (cells trace)
           (store cells (rest trace))]))

;; Without error checking

(define (map-trace-state-no-error [sto Store?]) Store?
  (type-case Store sto
    [store (cells trace)
           (type-case State (map-state-no-error (first trace) sto)
             [state (c adv mapped-sto)
                    (store (store-cells mapped-sto) (cons (state c adv (state-sto (first trace))) (rest trace)))])]))

(define (map-state-no-error [s State?] [sto Store?]) State?
  (type-case State s
    [state (c adv t-sto)
           (type-case Control c
             [interp-init () s]
             [app-call (abs args) 
                       (type-case ResultList (map-expr-list map-trace-value args sto)
                           (vs*s*t (vs-a s-a t-a)
                                  (state (app-call (rw-resume-value-no-error abs) vs-a) adv s-a)))]
             [app-result (r)
                         (type-case Result (map-trace-value r sto)
                           (v*s*t (v-r s-r t-r)
                                  (state (app-result v-r) adv s-r)))])]))

(define (rw-resume-value-no-error [v Value?]) Value?
  (deep-tag (all-tags v) (resumeV "dummy" 0)))

(define (rw-replay-call-no-error [abs Value?] [args (listof Value?)] [adv AdvStack?] [sto Store?]) Result?
  (apply-with-weaving abs args adv sto))

(define (rw-call-no-error [args (listof Value?)] [adv AdvStack?] [sto Store?]) Result?
  (rw-result-no-error adv (map-trace-state-no-error (next-trace-state sto))))

(define (rw-result-no-error [adv AdvStack?] [sto Store?]) Result?
  (type-case State (trace-state sto)
    [state (c t-adv t-sto)
           (type-case Control c
             [interp-init () 
                       (error 'rw-result-no-error "Unexpected state")]
             [app-call (abs args) 
                       (let ([result (rw-replay-call-no-error abs args adv sto)])
                         (rw-result-no-error adv (map-trace-state-no-error (next-trace-state (v*s*t-s result)))))]
             [app-result (r) 
                         (v*s*t r sto mt-trace)])]))

;; With error checking 

(define (map-trace-state [sto Store?]) Store?
  (type-case Store sto
    [store (cells trace)
           (type-case State (map-state (first trace) sto)
             [state (c adv mapped-sto)
                    (store (store-cells mapped-sto) (cons (state c adv (state-sto (first trace))) (rest trace)))])]))

(define (map-state [s State?] [sto Store?])
  (type-case State s
    [state (c adv t-sto)
           (type-case Control c
             [interp-init () s]
             [app-call (abs args) 
                       (type-case ResultList (map-expr-list map-trace-value args sto)
                           (vs*s*t (vs-a s-a t-a)
                                  (state (app-call (rw-resume-value abs s-a) vs-a) adv s-a)))]
             [app-result (r)
                         (type-case Result (map-trace-value r sto)
                           (v*s*t (v-r s-r t-r)
                                  (state (app-result v-r) adv s-r)))])]))

(define (rw-replay-call [abs Value?] [args (listof Value?)] [adv AdvStack?] [sto Store?]) Result?
  (rw-check-result (apply-with-weaving abs args adv sto)))

(define (rw-check-result [result Result?] [sto Store?]) Result?
  (type-case Result result
    [v*s*t (v-r s-r t-r)
           (let ([r (app-result-r (state-c (trace-state s-r)))])
             (if (equal-values v-r r)
                 result
                 (error 'retroactive-side-effect 
                        (format "incorrect retroactive result: expected\n ~a but got\n ~a" r v-r))))]))
  
(define (rw-resume-value [v Value?] [sto Store?]) Value?
  (type-case Store sto
    [store (cells t)
           (let ([r (resumeV (value->string v) (length t))])
             (deep-tag (all-tags v) r))]))

(define (rw-call [pos number?] [passed (listof Value?)] [adv AdvStack?] [sto Store?]) Result?
  (type-case Store sto
    [store (cells t)
           (cond [(= pos (length t))
                  (type-case State (first t)
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

(define (rw-result [adv AdvStack?] [sto Store?]) Result?
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
                           (rw-result adv (map-trace-state (next-trace-state (v*s*t-s result)))))]
               [app-result (r) 
                           (v*s*t r sto mt-trace)])])))