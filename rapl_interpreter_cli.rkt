#lang plai

(require "rapl.rkt")
(require "rapl_interpreter.rkt")
(require "rapl_parser.rkt")
(require "rapl_serialization.rkt")

;;
;; Rapl interpreter CLI
;;

(define eval-expr (make-parameter #f))
(define file-paths (make-parameter '()))
(define recording-path (make-parameter #f))
(define replay-path (make-parameter #f))
(define trace-path (make-parameter #f))
(define query-path (make-parameter #f))
(define verbose (make-parameter #f))
(define unsafe (make-parameter #f))

(command-line 
 #:program "ttpl"
 #:once-each
 [("-e" "--eval") expr "Evaluate expression" (eval-expr expr)]
 ;[("-r" "--record") path "Record execution" (recording-path path)]
 ;[("-p" "--replay") path "Replay execution" (replay-path path)]
 [("-t" "--trace") path "Trace execution" (trace-path path)]
 [("-w" "--weave") path "Retroactive weaver" (query-path path)]
 [("-v" "--verbose") "Verbose interpretation" (verbose true)]
 [("-u" "--unsafe") "Unsafe interpretation (no retroactive errors)" (unsafe true)]
 #:args sources (file-paths sources))

(set-box! verbose-interp (verbose))
(set-box! retroactive-error-checking (not (unsafe)))

(define (interp-program [exps list?]) Value?
  (interp-exp (app-chain exps)))

(define exps
  (cond
    [(eval-expr)
     (list (parse-string (eval-expr)))]
    [else
     (map parse-file (file-paths))]))

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

