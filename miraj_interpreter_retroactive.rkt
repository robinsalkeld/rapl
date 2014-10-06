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
  (strV (s string?))
  (closV (arg symbol?) (body ExprC?) (env Env?))
  (boxV (l Location?))
  (taggedV (tag Value?) (value Value?))
  (proceedV (label string?)))

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
    [proceedV (label)
              (type-case Store sto
                [store (cells trace)
                       (retroactive-weave-call (first trace) adv (store cells (rest trace)))])]
    [else (error 'interp "only abstractions can be applied")]))

;; Mutations and side-effects

(define-type Storage
  [cell (location Location?) (val Value?)])
 
(define (fetch [sto Store?] [loc Location?]) Value?
  (cond
    [(empty? sto) (error 'fetch "location not found")]
    [else (cond
            [(= loc (cell-location (first sto)))
             (cell-val (first sto))]
            [else (fetch (rest sto) loc)])]))

(define-type Store 
  [store (cells (listof Storage?)) (t Trace?)])
  
(define (new-loc [sto Store?]) Location?
  (length sto))

(define (override-store [sto Store?] [loc Location?] [value Value?])
  (cons (cell loc value) sto))

(define mt-store empty)

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
  [aroundAppV (value Value?)]
  [aroundSetV (value Value?)])
(define AdvEnv? (listof Advice?))  
(define mt-adv empty)

(define (apply-around-app [adv AdvEnv?] [advice Advice?] [abs-sto Result?]) Result?
  (type-case Advice advice
    [aroundAppV (f)
                (type-case Result abs-sto
                  (v*s*t (abs sto t) 
                         ;; TODO-RS: need to append result traces
                         (interp-closure-app f abs adv sto)))]
    [else abs-sto]))

(define (weave [adv AdvEnv?] [f Value?] [sto Store?]) Result?
  (foldr (curry apply-around-app adv) (v*s*t f sto) adv))

(define-type JoinPoint
  [app-call (abs Value?) (arg Value?) (adv AdvEnv?) (sto Store?)]
  [app-return (abs Value?) (result Value?) (adv AdvEnv?) (sto Store?)])
(define Joinpoints? (listof JoinPoint?))

(define (interp-app [abs Value?] [arg Value?] [adv AdvEnv?] [sto Store?]) Result?
  (type-case Result (weave adv abs sto)
      (v*s*t (woven-abs s-w t-w)
             (type-case Result (interp-closure-app woven-abs arg adv s-w)
                 (v*s*t (v s-v t-v)
                        (let ([call-jp (app-call abs arg adv sto)]
                              [return-jp (app-return abs v adv s-v)])
                            (v*s*t v s-v (append (list call-jp) t-w t-v (list return-jp)))))))))

;; Debugging

(define (display-value [v Value?] [out output-port?])
  (type-case Value v
    [numV (n) (display n out)]
    [boolV (b) (display b out)]
    [strV (s) (write s out)]
    [closV (arg body env)
           (begin (display arg out) (display " -> " out) (display (exp-syntax body) out) (display-env env out))]
    [boxV (l)
          (begin (display "box(" out) (display l out) (display ")" out))]
    [taggedV (t v)
            (begin (display "(tag " out) (display-value t out) (display " " out) (display-value v out) (display ")" out))]
    [proceedV (label) (display label out)]))

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
                 (begin (display "\t" out) (display l out) (display " -> " out) (display-value v out) (display "\n" out))])) 
       sto))

(define (display-joinpoint [jp JoinPoint?] [out output-port?])
  (type-case JoinPoint jp
    [app-call (abs arg adv sto)
              (begin (display "(app-call " out) (display-value abs out) (display ")" out))]
    [app-return (abs result adv sto)
                (begin (display "(app-return " out) (display-value result out) (display ")" out))]))
    

(define (display-with-label [label string?] [val Value?] [out output-port?])
  (begin (display label out) (display ": " out) (display-value val out) (newline out)))

;; Main interpretation function

(define verbose-interp (box false))

(define (interp [expr ExprC?] [env Env?] [adv AdvEnv?] [sto Store?]) Result?
(begin 
  (if (unbox verbose-interp)
      (begin
        (display "Expression: ") (display (exp-syntax expr)) (newline)
        ;;(display-context (e*s env sto)) 
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
                                   (v*s*t (v-b s-b (append t-c t-b)))])])]
    
    ;; Identifiers and abstractions
    
    [idC (n) (v*s*t (fetch sto (lookup n env)) sto mt-trace)]
    
    [lamC (a b) (v*s*t (closV a b env) sto)]
    
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
    
    [strC (s) (v*s*t (strV s) sto mt-trace)]
    
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
                                         
                         
    [aroundAppC (f in) 
                (type-case Result (interp f env adv sto)
                  [v*s*t (v-f s-f t-f)
                         (prepend-trace t-f (interp in env (cons (aroundAppV v-f) adv) s-f))])]
                
    
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

(define Trace? (listof JoinPoint?))
(define mt-trace empty)

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
                    (type-case Result (interp-app v-f v-a mt-env mt-adv s-a)
                      [v*s*t (v-r s-r t-r)
                             (let* ([trace (mirajTrace t-f t-a t-r)]
                                    [_ (write-struct-to-file trace trace-path)])
                               v-r)])])]))
       
(define-type LocationMap
  [locmap (from-loc Location?) (to-loc Location?)])

(define Mapping? (listof LocationMap?))
(define mt-mapping empty)

(define (mapped-location [m Mapping?] [loc Location?]) Location?
  (cond
    [(empty? m) #f]
    [else (cond
            [(= loc (locmap-from-loc (first m)))
             (locmap-to-loc (first m))]
            [else (mapped-location (rest m) loc)])]))

(define (override-mapping [m Mapping?] [from-loc Location?] [to-loc Location?]) Mapping?
  (cons (locmap from-loc to-loc) m))

(define-type CopyResult
  [v*s*m (v Value?) (s Store?) (m Mapping?)])

(define (copy-location [from-loc Location?] [from-sto Store?] [to-sto Store?] [m Mapping?]) CopyResult?
  (type-case CopyResult (copy-value (fetch from-sto from-loc) from-sto to-sto m)
    [v*s*m (v s-v m-v)
           (let* ([mapped-loc (mapped-location m from-loc)]
                  [to-loc (if mapped-loc mapped-loc (new-loc to-sto))]
                  [new-sto (override-store to-sto to-loc v)])
             (v*s*m (boxV to-loc) new-sto m))]))

(define (copy-binding [from-sto Store?] [b Binding?] [result CopyResult?]) CopyResult?
  (type-case Binding b
    [bind (name loc)
          (type-case CopyResult result
            [v*s*m (c sto map)
                   (type-case Value c
                     [closV (arg body env)
                            (type-case CopyResult (copy-location loc from-sto sto map)
                              [v*s*m (box s-box m-box)
                                     (v*s*m (closV arg body (cons (bind name (boxV-l box)) env)) s-box m-box)])]
                     [else (error 'copy-binding "result parametmer must wrap a closure")])])]))

(define (copy-closure [arg symbol?] [body ExprC?] [env Env?] [from-sto Store?] [to-sto Store?] [m Mapping?]) CopyResult?
  (foldr (curry copy-binding from-sto) (v*s*m (closV arg body mt-env) to-sto m) env))
                  
(define (copy-value (v Value?) (from-sto Store?) (to-sto Store?) (m Mapping?)) CopyResult?
  ;; TODO-RS: Need cycle detection because of boxes
  (type-case Value v
    [numV (_) 
          (v*s*m v to-sto m)]
    [boolV (_) 
          (v*s*m v to-sto m)]
    [strV (_) 
          (v*s*m v to-sto m)]
    [closV (arg body env)
           (copy-closure arg body env from-sto to-sto m)]
    [boxV (loc)
          (copy-location loc from-sto to-sto m)]
    [taggedV (tag tagged) 
             (type-case CopyResult (copy-value tag from-sto to-sto m)
               [v*s*m (copied-tag s-tag m-tag)
                      (type-case CopyResult (copy-value tagged from-sto s-tag m-tag)
                        [v*s*m (copied-tagged s-tagged m-tagged)
                               (v*s*m (taggedV copied-tag copied-tagged) s-tagged m-tagged)])])]
    [proceedV (_) (v*s*m v to-sto m)]))


(define (deep-equal-values (left Value?) (left-sto Store?) (right Value?) (right-sto Store?)) boolean?
  (type-case Value left
    [numV (_)
          (equal? left right)]
    [boolV (_) 
          (equal? left right)]
    [strV (_) 
          (equal? left right)]
    [closV (arg body env)
           (and (equal? left right)
                ;; This is probably broken given the override implementation
                (andmap (lambda (left-bind right-bind) 
                          (let* ([left-loc (bind-loc left-bind)]
                                 [left-value (fetch left-sto left-loc)]
                                 [right-loc (bind-loc right-bind)]
                                 [right-value (fetch right-sto right-loc)])
                            (deep-equal-values left-value left-sto right-value right-sto)))
                        env (closV-env right)))]
    [boxV (left-loc)
          (deep-equal-values (fetch left-sto left-loc) left-sto (fetch right-sto (boxV-l right)) right-sto)]
    [taggedV (left-tag left-tagged)
             (and (deep-equal-values left-tag left-sto (taggedV-tag right) right-sto)
                  (deep-equal-values left-tagged left-sto (taggedV-value right) right-sto))]
    [proceedV (_) (equal? left right)]))

(define (append-binding (context Context?) (b Binding?) (b-sto Store?)) Context?
  (type-case Context context
    [e*s (env sto)
         (type-case Binding b
           [bind (name loc)
                 (let ([where (new-loc sto)])
                   (e*s (cons (bind name where) env) 
                        (override-store sto where (fetch b-sto loc))))])]))
                     
(define (copy-env (left-c Context?) (right-c Context?)) 
  (type-case Context right-c
    (e*s (right-env right-sto)
         ; TODO-RS: foldl or foldr?
         (foldl (lambda (b c) (append-binding c b right-sto)) left-c right-env))))

(define miraj-ns (module->namespace "miraj_interpreter_retroactive.rkt"))

(define (interp-query (trace-path path-string?) (exprs list?)) Value?
  (type-case MirajTrace (read-struct-from-file miraj-ns trace-path)
    [mirajTrace (f-jps a-jps app-jps)
                (let* ([_ (set-box! read-source (lambda () (error 'retroactive-side-effect "cannot call read in retroactive advice")))]
                       [query-result (interp (app-chain exprs) mt-env mt-adv mt-store)]
                       [weave-closure (proceedV "interp-query")]
                       [x (interp-closure-app (v*s*t-v query-result) weave-closure mt-adv (v*s*t-s query-result))]
                       [result (interp-closure-app (v*s*t-v x) (numV 0) mt-adv (v*s*t-s x))])
                  (v*s*t-v result))]))

(define (all-tags [v Value?]) (listof Value?)
  (type-case Value v
    [taggedV (tag tagged)
             (cons tag (all-tags tagged))]
    [else '()]))

(define (deep-tag [tags (listof Value?)] [v Value?]) Value?
  (foldr taggedV v tags))

(define (retroactive-weave-call [jp JoinPoint?] [adv AdvEnv?] [sto Store?]) Result?
  (type-case JoinPoint jp
    [app-call (abs arg jp-adv jp-sto)
              (let* ([out (open-output-string)]
                     [_ (display-value abs out)]
                     [label (get-output-string out)]
                     [abs-copy-result (copy-value abs jp-sto sto mt-mapping)]
                     [arg-copy-result (copy-value arg jp-sto (v*s*m-s abs-copy-result) mt-mapping)]
                     [proceed-result (box #f)]
                     [proceed (lambda (val adv sto)
                                (if (unbox proceed-result)
                                    (error 'retroactive-side-effect "retroactive advice proceeded more than once")
                                    (if (boolV-b (equal-values val (v*s*m-v arg-copy-result)))
                                        (begin 
                                          (set-box! proceed-result (retroactive-weave-return adv sto))
                                          (unbox proceed-result))
                                        (error 'retroactive-side-effect 
                                               (format "incorrect argument passed retroactively: expected\n ~a but got\n ~a" (v*s*m-v arg-copy-result) val)))))]
                     [proceed-value (builtinV label proceed)]
                     [tagged (deep-tag (all-tags abs) proceed-value)]
                     [woven-result (weave adv tagged (v*s*m-s arg-copy-result))]
                     [result (interp-closure-app (v*s-v woven-result) (v*s*m-v arg-copy-result) adv (v*s-s woven-result))])
                (if (not (unbox proceed-result))
                    (error 'retroactive-side-effect "retroactive advice failed to proceed")
                    (if (not (boolV-b (equal-values (v*s-v (unbox proceed-result)) (v*s-v result))))
                        (error 'retroactive-side-effect 
                               (format "incorrect retroactive result: expected\n ~a but got\n ~a" (v*s-v (unbox proceed-result)) (v*s-v result)))
                        ;; TODO-RS: Verify the store - check that f and a are still equal
                        result)))]
    [app-return (abs result jp-adv jp-sto) 
                (error 'retroactive-weave-call "Unexpected app-return")]))

(define (retroactive-weave-return [adv AdvEnv?] [sto Store?]) Result?
  (type-case Store sto
    [store (cells trace)
           (let* ([jp (first trace)]
                  [_ (if (unbox verbose-interp)
                         (begin
                           (display "Weaving joinpoint: ") (display-joinpoint jp (current-output-port)) (newline))
                         '())])
             (type-case JoinPoint jp
               [app-call (abs arg jp-adv jp-sto) 
                         (let ([result (retroactive-weave-call adv (store cells (tail trace)))])
                           (retroactive-weave-return jps adv (v*s-s result)))]
               [app-return (abs result jp-adv jp-sto) 
                           (v*s*t result sto mt-trace)]))]))