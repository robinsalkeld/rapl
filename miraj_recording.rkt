#lang plai

(require "miraj.rkt")
(require "miraj_interpreter.rkt")
(require "miraj_serialization.rkt")

(define miraj-ns (module->namespace "miraj_recording.rkt"))

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

(define JoinPoints? (curry andmap JoinPoint?))

;; TODO-RS: Obviously needs generalizing beyond exactly two CLI inputs
  
(define-type MirajTrace
  [mirajTrace (f-jps JoinPoints?)
              (a-jps JoinPoints?)
              (app-jps JoinPoints?)])

(define (interp-with-tracing (exps list?) (trace-path path-string?))
  (let* ([f (interp (car exps) mt-env mt-adv mt-store)]
         [f-jps (get-interp-jps)]
         [a (interp (car (cdr exps)) mt-env mt-adv (v*s-s f))]
         [a-jps (get-interp-jps)]
         [result (interp-app (v*s-v f) (v*s-v a) mt-adv (v*s-s a))]
         [app-jps (get-interp-jps)]
         [trace (mirajTrace f-jps a-jps app-jps)]
         [_ (write-struct-to-file trace trace-path)])
    (v*s-v result)))
       
(define-type LocationMap
  [locmap (from-loc Location?) (to-loc Location?)])

(define Mapping? (curry andmap LocationMap?))
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
  (type-case CopyResult (copy-value (fetch from-sto from-loc))
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
  (foldr (curry copy-binding from-sto) (v*s*m (closV arg body mt-env) to-sto m)) env)
                  
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
    [builtinV (label f) (v*s*m v to-sto m)]))


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
    [builtinV (label f) (equal? left right)]))

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

(define (interp-query (trace-path path-string?) (exprs list?)) Value?
  (type-case MirajTrace (read-struct-from-file miraj-ns trace-path)
    [mirajTrace (f-jps a-jps app-jps)
                (let* ([_ (set-box! read-source (lambda () (error 'retroactive-side-effect "cannot call read in retroactive advice")))]
                       [query-result (interp (app-chain exprs) mt-env mt-adv mt-store)]
                       [weave-closure (builtinV "interp-query" (lambda (val adv sto) (retroactive-weave-call (car app-jps) (box (cdr app-jps)) adv sto mt-mapping)))]
                       [x (interp-closure-app (v*s-v query-result) weave-closure mt-adv (v*s-s query-result))]
                       [result (interp-closure-app (v*s-v x) (numV 0) mt-adv (v*s-s x))])
                  (v*s-v result))]))

(define (all-tags [v Value?]) (curry andmap Value?)
  (type-case Value v
    [taggedV (tag tagged)
             (cons tag (all-tags tagged))]
    [else '()]))

(define (deep-tag [tags (curry andmap Value?)] [v Value?]) Value?
  (foldr taggedV v tags))

(define (retroactive-weave-call [jp JoinPoint?] [jps box?] [adv AdvEnv?] [sto Store?] [m Mapping?]) CopyResult?
  (type-case JoinPoint jp
    [app-call (abs arg jp-adv jp-sto)
              (let* ([out (open-output-string)]
                     [_ (display-value abs out)]
                     [label (get-output-string out)]
                     [abs-copy-result (copy-value jp-sto abs sto m)]
                     [arg-copy-result (copy-value jp-sto arg (v*s*m-s abs-copy-result) (v*s*m-m abs-copy-result))]
                     [proceed-result (box #f)]
                     [proceed (lambda (val adv sto)
                                (if (unbox proceed-result)
                                    (error 'retroactive-side-effect "retroactive advice proceeded more than once")
                                    (if (boolV-b (equal-values val (v*s-v arg-copy-result)))
                                        (begin 
                                          (set-box! proceed-result (retroactive-weave-return jps adv sto))
                                          (unbox proceed-result))
                                        (error 'retroactive-side-effect 
                                               (format "incorrect argument passed retroactively: expected\n ~a but got\n ~a" (v*s-v arg-copy-result) val)))))]
                     [proceed-value (builtinV label proceed)]
                     [tagged (deep-tag (all-tags abs) proceed-value)]
                     [woven-result (weave adv tagged (v*s-s arg-copy-result))]
                     [result (interp-closure-app (v*s-v woven-result) (v*s-v arg-copy-result) adv (v*s-s woven-result))])
                (if (not (unbox proceed-result))
                    (error 'retroactive-side-effect "retroactive advice failed to proceed")
                    (if (not (boolV-b (equal-values (v*s-v (unbox proceed-result)) (v*s-v result))))
                        (error 'retroactive-side-effect 
                               (format "incorrect retroactive result: expected\n ~a but got\n ~a" (v*s-v (unbox proceed-result)) (v*s-v result)))
                        ;; TODO-RS: Verify the store - check that f and a are still equal
                        (v*s*m (v*s-v result) (v*s-s result) (v*s*m-m abs-copy-result)))))]
    [app-return (abs result jp-adv jp-sto) 
                (error 'retroactive-weave-call "Unexpected app-return")]))

(define (retroactive-weave-return [jps box?] [adv AdvEnv?] [sto Store?] [m Mapping?]) CopyResult?
  (let* ([jp (list-box-pop! jps)]
         [_ (if (unbox verbose-interp)
                (begin
                  (display "Weaving joinpoint: ") (display-joinpoint jp (current-output-port)) (newline))
                '())])
    (type-case JoinPoint jp
      [app-call (abs arg jp-adv jp-sto) 
                (let ([result (retroactive-weave-call jp jps adv sto)])
                  (retroactive-weave-return jps adv (v*s-s result)))]
      [app-return (abs result jp-adv jp-sto) 
                  (v*s result sto)])))