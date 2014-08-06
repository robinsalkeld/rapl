#lang plai

(require "miraj.rkt")
(require "miraj_interpreter.rkt")
(require "miraj_serialization.rkt")

(define-type MirajRecording
  [mirajRecForReplay (program list?) (input list?)])

(define-type MirajTrace
  [mirajTrace (joinpoints list?) (result Result?)])

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
         [trace (mirajTrace jps result)]
         [_ (write-struct-to-file trace trace-path)])
    result))
       
; TODO-RS: append-def and append-context are probably not doing the right thing in edge cases yet

(define (append-def (context Context?) (def DefC?) (def-sto Store?)) Context?
  (type-case Context context
    [e*s (env sto)
         (type-case DefC def
           [bindC (name loc) 
                  (let ([where (new-loc sto)])
                    (e*s (cons (bindC name where) env) (override-store sto where (fetch def-sto loc))))]
           [else (e*s (cons def env) sto)])]))
                     
(define (append-context (left-c Context?) (right-c Context?)) Context?
  (type-case Context right-c
    (e*s (right-env right-sto)
         ; TODO-RS: foldl or foldr?
         (foldl (lambda (def c) (append-def c def right-sto)) left-c right-env))))

(define (interp-query (trace-path path-string?) (exprs list?))
  (type-case MirajTrace (read-struct-from-file miraj-ns trace-path)
    [mirajTrace (jps result)
                (let* ([proceed (lambda (val env sto) (retroactive-weave (box jps) env sto))]
                       [_ (set-box! read-source (lambda () (error 'retroactive-side-effect "cannot call read in retroactive advice")))]
                       [_ (chain-interp exprs proceed)])
                  result)]))

(define (retroactive-weave [jps box?] [env Env?] [sto Store?]) Result?
  (cond
    [(empty? (unbox jps)) '()]
    [else 
     (type-case JoinPoint (list-box-pop! jps)
       [call (name arg jp-context) 
             (let* ([proceed (lambda (val new-env new-sto) 
                               ;; TODO-RS: Verify that val == arg!
                               ;; Not to mention verifying the same thing on return somehow.
                               (retroactive-weave jps new-env new-sto))]
                    [context (append-context (e*s env sto) jp-context)]
                    [woven-proceed (weave name (e*s-e context) proceed)]
                    [result (woven-proceed arg (e*s-e context) (e*s-s context))])
               (retroactive-weave jps env (v*s-s result)))]
       [return (name result jp-context) 
               (v*s result sto)])]))

               