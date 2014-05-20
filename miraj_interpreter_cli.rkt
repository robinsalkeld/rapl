#lang plai

(require "miraj.rkt")
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

(define exps (map (curry read-struct-from-file miraj-ns) (files)))

(cond
  [(recording-path) 
   (write (v*s-v (interp-with-recording exps (recording-path))))]
  [(replay-path) 
   (write (v*s-v (replay-interp (replay-path))))]
  [(trace-path) 
   (write (v*s-v (interp-with-tracing exps (trace-path))))]
  [(query-path) 
   (interp-query (query-path) exps)]
  [else 
   (write (v*s-v (chain-interp exps no-proceed)))])

