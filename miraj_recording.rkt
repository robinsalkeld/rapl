#lang plai

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
        [miraj (fds ads exp) 
           (weave-trace jps fds ads)])]))
                            
(define (weave-trace (jps list?) (fds FunEnv?) (ads AdvEnv?))
  (weave-trace-box (box jps) fds ads mt-store))

(define (weave-trace-box (jps box?) (fds FunEnv?) (ads AdvEnv?) (advice-store Store?)) 
  (cond
    [(empty? (unbox jps)) empty]
    [else 
     (type-case JoinPoint (list-box-pop! jps)
       [call (name arg jp-store) 
             (let* ([proceed (lambda (val sto) 
                               ;; TODO-RS: Verify that val == arg!
                               ;; Not to mention verifying the same thing on return somehow.
                               (weave-trace-box jps fds ads sto))]
                    [result ((weave name fds ads proceed) arg advice-store)])
               (weave-trace-box jps fds ads (v*s-s result)))]
       [return (name result) (v*s (v*s-v result) advice-store)])]))

               