#lang plai

(require "miraj.rkt")
(require "miraj_interpreter.rkt")
(require "miraj_serialization.rkt")


(define (interp-with-recording (program MirajProgram?) (recording-path path-string?))
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
         
(define (interp-with-tracing (program MirajProgram?) (trace-path path-string?))
  (let* ([result (interp-program program)]
         [jps (get-interp-jps)]
         [trace (mirajTrace program jps)]
         [_ (write-struct-to-file trace trace-path)])
    result))
           
(define (interp-query (trace-path path-string?) (program MirajProgram?))
  (type-case MirajTrace (read-struct-from-file trace-path) 
    [mirajTrace (trace-program jps)
      (type-case MirajProgram (append-programs trace-program program)
        [miraj (vars fds ads exp) 
               ;; This is wrong - should not be re-defining variables from the original program.
               ;; I'm not yet combining the two stores together. There should be a combination
               ;; store that allows reading from either but only writing to the advice store.
               (with-defs vars fds ads mt-env mt-store (lambda (env sto)
                                                         (retroactive-weave jps env sto)))])]))
                            
(define (retroactive-weave [jps list?] [env Env?] [sto Store?])
  (begin (set-box! read-source (lambda () (error 'retroactive-side-effect "cannot call read in retroactive advice")))
         (retroactive-weave-box (box jps) env sto)))

(define (retroactive-weave-box [jps box?] [env Env?] [advice-store Store?]) 
  (cond
    [(empty? (unbox jps)) '()]
    [else 
     (type-case JoinPoint (list-box-pop! jps)
       [call (name arg jp-store) 
             (let* ([proceed (lambda (val sto) 
                               ;; TODO-RS: Verify that val == arg!
                               ;; Not to mention verifying the same thing on return somehow.
                               (retroactive-weave-box jps env sto))]
                    [result ((weave name (envV-ads env) proceed) arg advice-store)])
               (retroactive-weave-box jps env (v*s-s result)))]
       [return (name result) (v*s (v*s-v result) advice-store)])]))

               