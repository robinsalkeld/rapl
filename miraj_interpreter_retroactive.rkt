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
  (closV (arg symbol?) (body ExprC?) (env Env?))
  (boxV (l Location?))
  (taggedV (tag Value?) (value Value?))
  (resumeV (label string?) (pos number?)))

;; TODO-RS: not quite a continuation, but similar
(define-type Cont
  [app-call (abs Value?) (arg Value?)]
  [app-result (abs Value?) (r Value?)])

(define-type JoinPoint
  [joinpoint (c Cont?) (adv AdvEnv?) (sto Store?)])
(define Trace? (listof JoinPoint?))
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
  (boolV (equal? l r)))
     
;; Identifiers and functions

(define-type Binding
  [bind (name symbol?) (loc Location?)])
(define Location? number?)
(define Env? (listof Binding?))
(define mt-env empty)

(define (lookup [for symbol?] [env Env?])
  (cond
    [(empty? env) (error 'lookup (string-append "name not found: " (symbol->string for)))]
    [else 
     (type-case Binding (first env)
       [bind (name loc)
             (cond
               [(symbol=? for name) loc]
               [else (lookup for (rest env))])])]))

(define (interp-with-binding [name symbol?] [a Value?] [expr ExprC?] [env Env?] [adv AdvEnv?] [sto Store?]) Result?
  (let* ([where (new-loc sto)]
         [new-env (cons (bind name where) env)]
         [new-sto (override-store sto where a)])
            (interp expr new-env adv new-sto)))

(define (interp-closure-app [closure Value?] [a Value?] [adv AdvEnv?] [sto Store?]) Result? 
  (type-case Value (deep-untag closure)
    [closV (arg body env)
           (interp-with-binding arg a body env adv sto)]
    [resumeV (label pos)
             (type-case Store sto
               [store (cells t)
                      (retroactive-weave-call pos a adv sto)])]
    [else (error 'interp (string-append "only abstractions can be applied: " (value->string closure)))]))

;; Mutations and side-effects

(define-type Storage
  [cell (location Location?) (val Value?)]
  [mapping (location Location?) (trace-loc Location?)])
 
(define (fetch-from-cells [cells (listof Storage?)] [sto Store?] [loc Location?]) Value?
  (cond
    [(empty? cells) (error 'fetch "location not found")]
    [else 
     (type-case Storage (first cells)
       [cell (l val)
             (cond
               [(= loc l) val]
               [else (fetch-from-cells (rest cells) sto loc)])]
       [mapping (l t-loc)
             (cond
               [(= loc l) 
                (let* ([trace-value (fetch (trace-store sto) t-loc)])
                  (type-case Result (map-value trace-value sto)
                    [v*s*t (v s-v t-v) v]))]
               [else (fetch-from-cells (rest cells) sto loc)])])]))

(define (fetch [sto Store?] [loc Location?]) Value?
  (type-case Store sto
    [store (cells t)
           (fetch-from-cells cells sto loc)]))

(define-type Store 
  [store (cells (listof Storage?)) (t Trace?)])

(define (trace-store [s Store?]) Store?
  (joinpoint-sto (first (store-t s))))

(define (new-loc [sto Store?]) Location?
  (type-case Store sto
    [store (cells t)
           (length cells)]))

(define (override-store [sto Store?] [loc Location?] [value Value?]) Store?
  (type-case Store sto
    [store (cells trace)
           (store (cons (cell loc value) cells) trace)]))

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

;; The result value will always be a box
(define (map-location [t-loc Location?] [sto Store?]) Result?
  (type-case Store sto
    [store (cells t)
           (let ([mapped-loc (mapped-location cells t-loc)])
             (if mapped-loc
                 (v*s*t (boxV mapped-loc) sto mt-trace)
                 (let* ([to-loc (new-loc sto)]
                        [sto2 (store (cons (mapping to-loc t-loc) cells) t)]
                        [value-result (map-value (fetch (trace-store sto) t-loc) sto2)])
                   (v*s*t (boxV to-loc) (v*s*t-s value-result) mt-trace))))]))

(define (map-binding [b Binding?] [result Result?]) Result?
  (type-case Binding b
    [bind (name loc)
          (type-case Result result
            [v*s*t (c sto t)
                   (type-case Value c
                     [closV (arg body env)
                            (type-case Result (map-location loc sto)
                              [v*s*t (box s-box t-box)
                                     (v*s*t (closV arg body (cons (bind name (boxV-l box)) env)) s-box mt-trace)])]
                     [else (error 'map-binding "result parametmer must wrap a closure")])])]))

(define (map-closure [arg symbol?] [body ExprC?] [env Env?] [sto Store?]) Result?
  (foldr map-binding (v*s*t (closV arg body mt-env) sto mt-trace) env))
                  
(define (map-value (v Value?) (sto Store?)) Result?
  (type-case Value v
    [numV (_) 
          (v*s*t v sto mt-trace)]
    [boolV (_) 
          (v*s*t v sto mt-trace)]
    [symbolV (_) 
          (v*s*t v sto mt-trace)]
    [closV (arg body env)
           (map-closure arg body env sto)]
    [boxV (loc)
          (map-location loc sto)]
    [taggedV (tag tagged) 
             (type-case Result (map-value tag sto)
               [v*s*t (mapped-tag s-tag t-tag)
                      (type-case Result (map-value tagged s-tag)
                        [v*s*t (mapped-tagged s-tagged t-tagged)
                               (v*s*t (taggedV mapped-tag mapped-tagged) s-tagged mt-trace)])])]
    [resumeV (label pos) (v*s*t v sto mt-trace)]))

(define-type Result
  [v*s*t (v Value?) (s Store?) (t Trace?)])

(define (prepend-trace [t Trace?] [r Result?]) Result?
  (type-case Result r
    [v*s*t (v-r s-r t-r)
           (v*s*t v-r s-r (append t t-r))]))

(define-type Context
  [e*s (e Env?) (s Store?)])

;; Advice

(define-type Advice
  [onappV (value Value?)]
  [aroundSetV (value Value?)])
(define AdvEnv? (listof Advice?))  
(define mt-adv empty)

(define (apply-onapp [adv AdvEnv?] [advice Advice?] [abs-sto Result?]) Result?
  (type-case Advice advice
    [onappV (f)
            (type-case Result abs-sto
              (v*s*t (abs sto t) 
                     (prepend-trace t (interp-closure-app f abs adv sto))))]
    [else abs-sto]))

(define (weave [adv AdvEnv?] [f Value?] [sto Store?]) Result?
  (foldr (curry apply-onapp adv) (v*s*t f sto mt-trace) adv))

(define (interp-app [abs Value?] [arg Value?] [adv AdvEnv?] [sto Store?]) Result?
  (type-case Result (weave adv abs sto)
      (v*s*t (woven-abs s-w t-w)
             (type-case Result (interp-closure-app woven-abs arg adv s-w)
                 (v*s*t (v s-v t-v)
                        (let ([call-jp (joinpoint (app-call abs arg) adv sto)]
                              [return-jp (joinpoint (app-result abs v) adv s-v)])
                            (v*s*t v s-v (append (list call-jp) t-w t-v (list return-jp)))))))))

;; Debugging

(define (display-value [v Value?] [out output-port?])
  (type-case Value v
    [numV (n) (display n out)]
    [boolV (b) (display b out)]
    [symbolV (s) (write s out)]
    [closV (arg body env)
           (begin (display arg out) (display " -> " out) (display (exp-syntax body) out) (display-env env out))]
    [boxV (l)
          (begin (display "box(" out) (display l out) (display ")" out))]
    [taggedV (t v)
            (begin (display "(tag " out) (display-value t out) (display " " out) (display-value v out) (display ")" out))]
    [resumeV (label f) (display label out)]))

(define (value->string [v Value?]) string?
  (letrec ([out (open-output-string)]
           [_ (display-value v out)])
    (get-output-string out)))

(define (display-context [c Context?] [out output-port?])
  (begin (display "Environment: \n" out)
         (display-env (e*s-e c) out)
         (display "Store: \n" out)
         (display-store (e*s-s c) out)))
         
(define (display-env [env Env?] [out output-port?])
  (map (lambda (def) 
         (type-case Binding def
           [bind (n l)
                 (begin (display "\t" out) (display n out) (display " -> " out) (display l out) (display "\n" out))])) 
       env))

(define (display-store [sto Store?] [out output-port?])
  (map (lambda (c) 
         (type-case Storage c
           [cell (l v)
                 (begin (display "\t" out) (display l out) (display " -> " out) (display-value v out) (display "\n" out))]
           [mapping (l t-l)
                    (begin (display "\t" out) (display l out) (display " -> [" out) (display t-l out) (display "]\n" out))])) 
       (store-cells sto)))

(define (display-joinpoint [jp JoinPoint?] [out output-port?])
  (type-case JoinPoint jp
    [joinpoint (c adv sto)
               (type-case Cont c
                 [app-call (abs arg)
                           (begin (display "(app-call " out) (display-value abs out) (display ")" out))]
                 [app-result (abs result)
                             (begin (display "(app-return " out) (display-value result out) (display ")" out))])]))
    

(define (display-with-label [label string?] [val Value?] [out output-port?])
  (begin (display label out) (display ": " out) (display-value val out) (newline out)))

;; Main interpretation function

(define verbose-interp (box false))

(define (interp [expr ExprC?] [env Env?] [adv AdvEnv?] [sto Store?]) Result?
(begin 
  (if (unbox verbose-interp)
      (begin
        (display "Expression: ") (display (exp-syntax expr)) (newline)
        (display-context (e*s env sto) (current-output-port)) 
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
                               (v*s*t (equal-values v-l v-r) s-r (append t-l t-r))])])]
    
    [ifC (c t f) (type-case Result (interp c env adv sto)
                   [v*s*t (v-c s-c t-c)
                          (type-case Result (if (boolV-b v-c)
                                                (interp t env adv s-c)
                                                (interp f env adv s-c))
                            [v*s*t (v-b s-b t-b) 
                                   (v*s*t v-b s-b (append t-c t-b))])])]
    
    ;; Identifiers and abstractions
    
    [idC (n) (v*s*t (fetch sto (lookup n env)) sto mt-trace)]
    
    [lamC (a b) (v*s*t (closV a b env) sto mt-trace)]
    
    [appC (f a) (type-case Result (interp f env adv sto)
                  [v*s*t (v-f s-f t-f)
                         (type-case Result (interp a env adv s-f)
                           [v*s*t (v-a s-a t-a) 
                                  (type-case Result (interp-app v-f v-a adv s-a)
                                    [v*s*t (v-r s-r t-r)
                                           (v*s*t v-r s-r (append t-f t-a t-r))])])])]
    
    [letC (s v in) (type-case Result (interp v env adv sto)
                            [v*s*t (v-v s-v t-v)
                                 (prepend-trace t-v (interp-with-binding s v-v in env adv s-v))])]
    
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
    
    ;; Advice
    
    [symbolC (s) (v*s*t (symbolV s) sto mt-trace)]
    
    [tagC (tag v) 
          (type-case Result (interp tag env adv sto)
            [v*s*t (v-tag s-tag t-tag)
                   (type-case Result (interp v env adv s-tag)
                     [v*s*t (v-v s-v t-v)
                            (v*s*t (taggedV v-tag v-v) s-v (append t-tag t-v))])])]
    
    [tagtestC (v f g)
              (type-case Result (interp v env adv sto)
                  [v*s*t (v-v s-v t-v)
                         (type-case Value v-v
                           [taggedV (tag tagged)
                                    (type-case Result (interp f env adv s-v)
                                      [v*s*t (v-f s-f t-f)
                                             (type-case Result (interp-app v-f tag adv s-f)
                                               [v*s*t (v-f2 s-f2 t-f2)
                                                      (prepend-trace (append t-v t-f t-f2)
                                                                     (interp-app v-f2 tagged adv s-f2))])])]
                           [else (prepend-trace t-v (interp g env adv s-v))])])]
                                         
                         
    [onappC (wrapper scope) 
            (type-case Result (interp wrapper env adv sto)
              [v*s*t (v-w s-w t-w)
                     (prepend-trace t-w (interp scope env (cons (onappV v-w) adv) s-w))])]
    
    [aroundSetC (f in) 
                (type-case Result (interp f env adv sto)
                  [v*s*t (v-f s-f t-f)
                         (prepend-trace t-f (interp in env (cons (aroundSetV v-f) adv) s-f))])]
    
    ;; Input/Output
    
    [fileC (path) (interp (parse-file path) mt-env adv sto)]
    
    [writeC (l a) (type-case Result (interp a env adv sto)
               [v*s*t (v-a s-a t-a) 
                      (begin (display-with-label l v-a (current-output-port)) 
                             (v*s*t v-a s-a t-a))])]
    
    [readC (l) (let* ([_ (display l)]
                      [_ (display "> ")]
                      [val ((unbox read-source))]
                      [_ (record-interp-input val)])
                 (v*s*t (numV val) sto mt-trace))]))
)

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
         [_ (set-box! read-source (lambda () (list-box-pop! remaining-input)))])
    ((interp-exp (app-chain (mirajRecForReplay-program recording))))))
         
;; Tracing

;; TODO-RS: Obviously needs generalizing beyond exactly two CLI inputs
  
(define-type MirajTrace
  [mirajTrace (f-jps Trace?)
              (a-jps Trace?)
              (app-jps Trace?)])

(define (interp-with-tracing (exps list?) (trace-path path-string?)) Value?
  (type-case Result (interp (first exps) mt-env mt-adv mt-store)
    [v*s*t (v-f s-f t-f)
           (type-case Result (interp (first (rest exps)) mt-env mt-adv s-f)
             [v*s*t (v-a s-a t-a)
                    (type-case Result (interp-app v-f v-a mt-adv s-a)
                      [v*s*t (v-r s-r t-r)
                             (let* ([trace (mirajTrace t-f t-a t-r)]
                                    [_ (write-struct-to-file trace trace-path)])
                               v-r)])])]))
       
(define miraj-ns (module->namespace "miraj_interpreter_retroactive.rkt"))

(define (interp-query (trace-path path-string?) (exprs list?)) Value?
  (type-case MirajTrace (read-struct-from-file miraj-ns trace-path)
    [mirajTrace (f-jps a-jps app-jps)
                (let* ([_ (set-box! read-source (lambda () (error 'retroactive-side-effect "cannot call read in retroactive advice")))]
                       [query-result (interp (app-chain exprs) mt-env mt-adv (store empty app-jps))]
                       [jp (first app-jps)]
                       [weave-closure (retroactive-resume-value (app-call-abs (joinpoint-c jp)) (v*s*t-s query-result))]
                       [x (interp-app (v*s*t-v query-result) weave-closure mt-adv (v*s*t-s query-result))]
                       [result (interp-app (v*s*t-v x) (numV 0) mt-adv (v*s*t-s x))])
                  (v*s*t-v result))]))

(define (all-tags [v Value?]) (listof Value?)
  (type-case Value v
    [taggedV (tag tagged)
             (cons tag (all-tags tagged))]
    [else '()]))

(define (deep-tag [tags (listof Value?)] [v Value?]) Value?
  (foldr taggedV v tags))

;; TODO-RS: Merge advice in scope properly wherever retroactive execution meets prior state

(define (pop-joinpoint [s Store?]) Store?
  (type-case Store s
    [store (cells trace)
           (store cells (rest trace))]))

(define (retroactive-replay-call [abs Value?] [arg Value?] [adv AdvEnv?] [sto Store?]) Result?
  (let ([resume (retroactive-resume-value abs sto)])
    (interp-app resume arg adv sto)))

(define (retroactive-resume-value [v Value?] [sto Store?]) Value?
  (type-case Store sto
    [store (cells t)
           (let ([r (resumeV (value->string v) (length t))])
             (deep-tag (all-tags v) r))]))

(define (retroactive-weave-call [pos number?] [a Value?] [adv AdvEnv?] [sto Store?]) Result?
  (type-case Store sto
    [store (cells t)
           (type-case JoinPoint (first t)
             [joinpoint (c jp-adv jp-sto)
                        (type-case Cont c
                          [app-call (abs arg)
                                    (cond [(= pos (length t))
                                           (let* ([abs-copy-result (map-value abs sto)]
                                                  [arg-copy-result (map-value arg (v*s*t-s abs-copy-result))])
                                             (if (or #t (boolV-b (equal-values a (v*s*t-v arg-copy-result))))
                                                 (retroactive-weave-result adv (pop-joinpoint sto))
                                                 (error 'retroactive-side-effect
                                                        (format "incorrect argument passed retroactively: expected\n ~a but got\n ~a" 
                                                                (v*s*t-v arg-copy-result) a))))]
                                          [else (error 'retroactive-side-effect "retroactive advice proceeded out of order")])]
                          [else (error 'retroactive-weave-call "Unexpected continuation")])])]))

(define (retroactive-weave-result [adv AdvEnv?] [sto Store?]) Result?
  (type-case Store sto
    [store (cells trace)
           (let* ([jp (first trace)]
                  [_ (if (unbox verbose-interp)
                         (begin
                           (display "Weaving joinpoint: ") (display-joinpoint jp (current-output-port)) (newline))
                         '())])
             (type-case JoinPoint jp
               [joinpoint (c jp-adv jp-sto)
                          (type-case Cont c
                            [app-call (abs arg) 
                                      (let ([result (retroactive-replay-call abs arg adv sto)])
                                        (retroactive-weave-result adv (pop-joinpoint (v*s*t-s result))))]
                            [app-result (abs result) 
                                        (v*s*t result sto mt-trace)])]))]))