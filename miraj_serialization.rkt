#lang plai

(define (struct->list/recursive v) list?
  (cond [(struct? v)
         (let* ([l (vector->list (struct->vector v))]
                [cons-symbol (string->symbol (string-replace (symbol->string (car l)) "struct:" ""))])
           (cons cons-symbol (map struct->list/recursive (cdr l))))]
        [(list? v) (list* 'list (map struct->list/recursive v))]
        ;; Double quote the symbol so it evaluates to the symbol
        [(symbol? v) `',v] 
        [else v]))

(define (list->struct/recursive (ns namespace?) (l list?)) struct?
  (eval l ns))

(define (write-struct-to-file (s struct?) (path path-string?))
  (call-with-output-file path (lambda (port) (pretty-write (struct->list/recursive s) port))))

(define (read-struct-from-file (ns namespace?) (path path-string?)) struct?
  (call-with-input-file path (lambda (port) (list->struct/recursive ns (read port)))))
