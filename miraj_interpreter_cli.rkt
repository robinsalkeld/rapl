#lang plai

(require "miraj.rkt")
(require "miraj_interpreter_retroactive.rkt")
(require "miraj_parser.rkt")
(require "miraj_serialization.rkt")

;;
;; Miraj interpreter CLI
;;

(define recording-path (make-parameter #f))
(define replay-path (make-parameter #f))
(define trace-path (make-parameter #f))
(define query-path (make-parameter #f))
(define file-paths (make-parameter '()))
(define verbose (make-parameter #f))

(command-line 
 #:program "miraj"
 #:once-each
 ;[("-r" "--record") path "Record execution" (recording-path path)]
 ;[("-p" "--replay") path "Replay execution" (replay-path path)]
 [("-t" "--trace") path "Trace execution" (trace-path path)]
 [("-q" "--query") path "Query execution" (query-path path)]
 [("-v" "--verbose") "Verbose interpretation" (verbose true)]
 #:args sources (file-paths sources))

(set-box! verbose-interp (verbose))

(define exps (map parse-file (file-paths)))

(define (interp-program [exps list?]) Value?
  (interp-exp (app-chain exps)))

(let ([result
       (cond
;         [(recording-path) 
;          (interp-with-recording exps (recording-path))]
;         [(replay-path) 
;          (replay-interp (replay-path))]
         [(trace-path) 
          (interp-with-tracing exps (trace-path))]
         [(query-path) 
          (interp-query (query-path) exps)]
         [else 
          (interp-program exps)])])
  (display-with-label "Program result" result (current-output-port)))

