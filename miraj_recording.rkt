#lang plai

(require "miraj.rkt")
(require "miraj_interpreter.rkt")
(require "miraj_serialization.rkt")


(define (interp-with-recording (exps list?) (recording-path path-string?))
  (let* ([result (interp-program exps)]
         [input (get-interp-input)]
         [recording (mirajRecForReplay exps input)]
         [_ (write-struct-to-file recording recording-path)])
    result))
           
(define (replay-interp (recording-path path-string?))
  (let* ([recording (read-struct-from-file recording-path)]
         [remaining-input (box (mirajRecForReplay-input recording))]
         [_ (set-box! read-source (lambda () (list-box-pop! remaining-input)))])
    (interp-program (mirajRecForReplay-program recording))))
         
(define (interp-with-tracing (exps list?) (trace-path path-string?))
  (let* ([result (interp-program exps)]
         [jps (get-interp-jps)]
         [trace (mirajTrace exps jps)]
         [_ (write-struct-to-file trace trace-path)])
    result))
      
(define-type HybridLocation
  [left-loc (loc Location?)]
  [right-loc (loc Location?)])
                  
(define (append-store (left-sto Store?) (right-sto Store?))
  (store (lambda () (right-loc (new-loc right-sto)))
         
         (lambda (loc) (type-case HybridLocation loc
                         [left-loc (l-loc) (fetch l-loc left-sto)]
                         [right-loc (r-loc) (fetch r-loc right-sto)]))
                         
         (lambda (loc value) (type-case HybridLocation loc
                         [left-loc (l-loc) (error 'hybrid-store "attempt to write to old location")]
                         [right-loc (r-loc) (append-store left-sto (override-store right-sto r-loc value))]))
         
         (lambda () (error 'not-implemented "todo"))
           
         (lambda (data) (error 'not-implemented "todo"))))

(define (interp-query (trace-path path-string?) (exprs list?))
  (type-case MirajTrace (read-struct-from-file trace-path) 
    [mirajTrace (trace-program jps)
                (let* ([proceed (lambda (val env sto) (retroactive-weave (box jps) env sto))]
                       [_ (set-box! read-source (lambda () (error 'retroactive-side-effect "cannot call read in retroactive advice")))])
                  (chain-interp exprs proceed))]))
                            
(define (retroactive-weave [jps box?] [env Env?] [sto Store?]) Result?
  (cond
    [(empty? (unbox jps)) '()]
    [else 
     (type-case JoinPoint (list-box-pop! jps)
       [call (name arg jp-env jp-sto-serialized) 
             (let* ([jp-sto (deserialize-store (list-store (list)) jp-sto-serialized)]
                    [proceed (lambda (val old-env old-sto) 
                               ;; TODO-RS: Verify that val == arg!
                               ;; Not to mention verifying the same thing on return somehow.
                               ;; TODO-RS: Need to make hybrid store!
                               (retroactive-weave jps env jp-sto))]
                    [result ((weave name env proceed) arg jp-env jp-sto)])
               (retroactive-weave jps env (v*s-s result)))]
       [return (name result jp-env jp-sto-serialized) 
               (v*s result sto)])]))

               