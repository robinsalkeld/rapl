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
       
(define (copy-value (sto Store?) (v Value?) (new-sto Store?)) Result?
  ;; TODO-RS: Need cycle detection because of boxes
  (type-case Value v
    [numV (_) 
          (v*s v new-sto)]
    [boolV (_) 
          (v*s v new-sto)]
    [strV (_) 
          (v*s v new-sto)]
    [closV (arg body env)
           (let ([copied-context (append-context (e*s mt-env new-sto) (e*s env sto))])
             (v*s (closV arg body (e*s-e copied-context)) (e*s-s copied-context)))]
    [boxV (loc)
          (let ([where (new-loc new-sto)]
                [result (copy-value sto (fetch sto loc) new-sto)])
            (v*s (boxV where) (override-store (v*s-s result) where (v*s-v result))))]
    [taggedV (tag tagged) 
             (let* ([tag-result (copy-value sto tag new-sto)]
                    [tagged-result (copy-value sto tagged (v*s-s tag-result))])
               (v*s (taggedV (v*s-v tag-result) (v*s-v tagged-result)) (v*s-s tagged-result)))]
    [builtinV (label f) (v*s v new-sto)]))

(define (append-binding (context Context?) (b Binding?) (b-sto Store?)) Context?
  (type-case Context context
    [e*s (env sto)
         (type-case Binding b
           [bind (name loc)
                 (let ([where (new-loc sto)])
                   (e*s (cons (bind name where) env) 
                        (override-store sto where (fetch b-sto loc))))])]))
                     
(define (append-context (left-c Context?) (right-c Context?)) Context?
  (type-case Context right-c
    (e*s (right-env right-sto)
         ; TODO-RS: foldl or foldr?
         (foldl (lambda (b c) (append-binding c b right-sto)) left-c right-env))))

(define (interp-query (trace-path path-string?) (exprs list?)) Value?
  (type-case MirajTrace (read-struct-from-file miraj-ns trace-path)
    [mirajTrace (f-jps a-jps app-jps)
                (let* ([_ (set-box! read-source (lambda () (error 'retroactive-side-effect "cannot call read in retroactive advice")))]
                       [query-result (interp (app-chain exprs) mt-env mt-adv mt-store)]
                       [weave-closure (builtinV "interp-query" (lambda (val adv sto) (retroactive-weave-call (car app-jps) (box (cdr app-jps)) adv sto)))]
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

(define (retroactive-weave-call [jp JoinPoint?] [jps box?] [adv AdvEnv?] [sto Store?]) Result?
  (type-case JoinPoint jp
    [app-call (abs arg jp-adv jp-sto) 
              (let* ([out (open-output-string)]
                     [_ (display-value abs out)]
                     [label (get-output-string out)]
                     [abs-copy-result (copy-value jp-sto abs sto)]
                     [arg-copy-result (copy-value jp-sto arg (v*s-s abs-copy-result))]
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
                        ;; TODO-RS: Verify the store 
                        result)))]
    [app-return (abs result jp-adv jp-sto) 
                (error 'retroactive-weave-call "Unexpected app-return")]))

(define (retroactive-weave-return [jps box?] [adv AdvEnv?] [sto Store?]) Result?
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