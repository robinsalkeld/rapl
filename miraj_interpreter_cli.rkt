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
(define file-paths (make-parameter '()))

(command-line 
 #:program "miraj"
 #:once-each
 [("-r" "--record") path "Record execution" (recording-path path)]
 [("-p" "--replay") path "Replay execution" (replay-path path)]
 [("-t" "--trace") path "Trace execution" (trace-path path)]
 [("-q" "--query") path "Query execution" (query-path path)]
 #:args sources (file-paths sources))

(define exps (map (curry read-struct-from-file miraj-ns) (file-paths)))

(let ([result 
       (cond
         [(recording-path) 
          (interp-with-recording exps (recording-path))]
         [(replay-path) 
          (replay-interp (replay-path))]
         [(trace-path) 
          (interp-with-tracing exps (trace-path))]
         [(query-path) 
          (interp-query (query-path) exps)]
         [else 
          (chain-interp exps no-proceed)])])
  (display-with-label "Program result: " (v*s-v result)))

