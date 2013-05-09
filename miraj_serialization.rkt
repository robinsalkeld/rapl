#lang plai

(require "miraj_interpreter.rkt")
(define miraj-ns (module->namespace "miraj_interpreter.rkt"))

(define (struct->list/recursive s) list?
  (if (struct? s)
  (let* ([l (vector->list (struct->vector s))]
        [cons-symbol (string->symbol (string-replace (symbol->string (car l)) "struct:" ""))]
        [recurse (lambda (v) 
                   (cond [(struct? v) (struct->list/recursive v)] 
                         [(list? v) (list* 'list (map struct->list/recursive v))]
                         ;; Double quote the symbol so it evaluates to the symbol
                         [(symbol? v) `',v] 
                         [else v]))])
        (cons cons-symbol (map recurse (cdr l))))
  s))

(define (list->struct/recursive (l list?)) struct?
  (eval l miraj-ns))

(define (write-struct-to-file (s struct?) (path path-string?))
  (call-with-output-file path (lambda (port) (write (struct->list/recursive s) port))))

(define (read-struct-from-file (path path-string?)) struct?
  (call-with-input-file path (lambda (port) (list->struct/recursive (read port)))))
