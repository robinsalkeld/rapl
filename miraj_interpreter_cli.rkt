#lang plai

(require "miraj_interpreter.rkt")
(require "miraj_serialization.rkt")
(require "miraj_recording.rkt")

;;
;; Miraj interpreter CLI
;;

(define recording-path (make-parameter #f))
(define replay-path (make-parameter #f))
(define trace-path (make-parameter #f))
(define query-path (make-parameter #f))
(define files (make-parameter '()))

(command-line 
 #:once-each
 [("-r" "--record") r "Record execution" (recording-path r)]
 [("-p" "--replay") r "Replay execution" (replay-path r)]
 [("-t" "--trace") r "Trace execution" (trace-path r)]
 [("-q" "--query") r "Query execution" (query-path r)]
 #:args fs (files fs))

(define program (lambda () (foldl append-programs mt-program (map read-struct-from-file (files)))))

(cond
  [(recording-path) 
   (begin (write (v*s-v (interp-with-recording (program) (recording-path)))))]
  [(replay-path) 
   (begin (write (v*s-v (replay-interp (replay-path)))))]
  [(trace-path) 
   (begin (write (v*s-v (interp-with-tracing (program) (trace-path)))))]
  [(query-path) 
   (interp-query (query-path) (program))]
  [else (write (v*s-v (interp-program (program))))])

