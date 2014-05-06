#lang plai

(require "miraj.rkt")
(require "miraj_interpreter.rkt")
(require "miraj_serialization.rkt")


(define (interp-with-recording (program ExprC?) (recording-path path-string?))
  (let* ([result (interp-program program)]
         [input (get-interp-input)]
         [recording (mirajRecForReplay program input)]
         [_ (write-struct-to-file recording recording-path)])
    result))
           
(define (replay-interp (recording-path path-string?))
  (let* ([recording (read-struct-from-file recording-path)]
         [remaining-input (box (mirajRecForReplay-input recording))]
         [_ (set-box! read-source (lambda () (list-box-pop! remaining-input)))])
    (interp-program (mirajRecForReplay-program recording))))
         
(define (interp-with-tracing (program ExprC?) (trace-path path-string?))
  (let* ([result (interp-program program)]
         [jps (get-interp-jps)]
         [trace (mirajTrace program jps)]
         [_ (write-struct-to-file trace trace-path)])
    result))
      
(define-type HybridLocation
  [left-loc (loc Location?)]
  [right-loc (loc Location?)])
   
(define (append-env (left-env Env?) (right-env Env?))
  (type-case Env left-env
    [envV (left-vars left-fds left-ads)
          (type-case Env right-env
            [envV (right-vars right-fds right-ads)
                  ;; TODO-RS
                  (envV left-vars left-fds 
                        (append left-ads right-ads))])]))
                  
(define (append-store (left-sto Store?) (right-sto Store?))
  (store (lambda () (right-loc (new-loc right-sto)))
         
         (lambda (loc) (type-case HybridLocation loc
                         [left-loc (l-loc) (fetch l-loc left-sto)]
                         [right-loc (r-loc) (fetch r-loc right-sto)]))
                         
         (lambda (loc value) (type-case HybridLocation loc
                         [left-loc (l-loc) (error 'hybrid-store "attempt to write to old location")]
                         [right-loc (r-loc) (append-store left-sto (override-store right-sto r-loc value))]))))

(define (interp-query (trace-path path-string?) (program ExprC?))
  (type-case MirajTrace (read-struct-from-file trace-path) 
    [mirajTrace (trace-program jps)
                (begin (set-box! read-source (lambda () (error 'retroactive-side-effect "cannot call read in retroactive advice")))
                       (interp program mt-env mt-store (lambda (val env sto)
                                                         (retroactive-weave (box jps) env sto))))]))
                            
(define (retroactive-weave [jps box?] [env Env?] [sto Store?]) 
  (cond
    [(empty? (unbox jps)) '()]
    [else 
     (type-case JoinPoint (list-box-pop! jps)
       [call (name arg) 
             ;; TODO-RS: Need to record env and store in joinpoint!
             (let* ([proceed (lambda (val old-env old-sto) 
                               ;; TODO-RS: Verify that val == arg!
                               ;; Not to mention verifying the same thing on return somehow.
                               (retroactive-weave jps env sto))]
                    [result ((weave name (envV-ads env) proceed) arg sto)])
               (retroactive-weave jps env (v*s-s result)))]
       [return (name result) (v*s result sto)])]))

               