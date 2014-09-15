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

(define-type MirajTrace
  [mirajTrace (joinpoints (curry andmap JoinPoint?)) (arity number?) (result Value?)])

(define (interp-with-tracing (exps list?) (trace-path path-string?))
  (let* ([result (interp-exp (app-chain exps))]
         [jps (get-interp-jps)]
         [trace (mirajTrace jps (length exps) result)]
         [_ (write-struct-to-file trace trace-path)])
    result))
       
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
                [result (copy-value (fetch sto loc) new-sto)])
            (v*s (boxV where) (override-store (v*s-s result) where (v*s-v result))))]
    [taggedV (tag tagged) 
             (let* ([tag-result (copy-value sto tag new-sto)]
                    [tagged-result (copy-value sto tagged (v*s-s tag-result))])
               (v*s (taggedV (v*s-v tag-result) (v*s-v tagged-result)) (v*s-s tagged-result)))]
    [builtinV (f) (v*s f new-sto)]))

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

(define (interp-query (trace-path path-string?) (exprs list?))
  (type-case MirajTrace (read-struct-from-file miraj-ns trace-path)
    [mirajTrace (jps arity result)
                (let* ([_ (set-box! read-source (lambda () (error 'retroactive-side-effect "cannot call read in retroactive advice")))]
                       [query-result (interp (app-chain exprs) mt-env mt-adv mt-store)]
                       [weave-closure (rw-closure (box jps))]
                       ;; TODO-RS: Fix hard-coded CLI arity and argument
                       [x (interp-closure-app (v*s-v query-result) weave-closure mt-adv (v*s-s query-result))]
                       [_ (interp-closure-app (v*s-v x) (numV 0) mt-adv (v*s-s x))])
                  result)]))

(define (retroactive-weave [jps box?] [adv AdvEnv?] [sto Store?]) Result?
  (cond
    [(empty? (unbox jps)) '()]
    [else 
      
      (let* ([jp (list-box-pop! jps)]
             [_ (if (unbox verbose-interp)
                    (begin
                      (display "Weaving joinpoint: ") (display-joinpoint jp) (newline))
                    '())])
        (type-case JoinPoint jp
          [app-call (abs arg jp-adv jp-sto) 
                    (let* (;; TODO-RS: Verify that val == arg!
                           ;; Not to mention verifying the same thing on return somehow.
                           [proceed (rw-closure jps)]
                           [abs-copy-result (copy-value jp-sto abs sto)]
                           [arg-copy-result (copy-value jp-sto arg (v*s-s abs-copy-result))]
                           [woven-result (weave adv proceed (v*s-s arg-copy-result))]
                           [result (interp-closure-app (v*s-v woven-result) (v*s-v arg-copy-result) adv (v*s-s woven-result))])
                      (retroactive-weave jps adv (v*s-s result)))]
          [app-return (abs result jp-adv jp-sto) 
                      (v*s result sto)]))]))

(define (rw-closure [jps box?]) Value?
  (builtinV (lambda (val adv sto) (retroactive-weave jps adv sto))))
               