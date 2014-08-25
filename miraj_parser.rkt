#lang plai

(require "miraj.rkt")

(define (parse s) ExprC?
;  (begin (print s) (newline)
  (cond [(list? s) 
         (let ([head (car s)]
               [tail (cdr s)])
           (case head
             ;; Numbers, arithmetic, and conditionals
             ['+
              (plusC (parse (list-ref tail 0)) (parse (list-ref tail 1)))]
             ['*
              (multC (parse (list-ref tail 0)) (parse (list-ref tail 1)))]
             ['if0
              (ifZeroC (parse (list-ref tail 0)) (parse (list-ref tail 1)) (parse (list-ref tail 2)))]
             ;; Identifiers and functions
             ['lambda
              (lamC (car (list-ref tail 0)) (parse (list-ref tail 1)))]
             ['let
              (let ([defpair (car (list-ref tail 0))])
                (letC (list-ref defpair 0) (parse (list-ref defpair 1)) (parse (list-ref tail 1))))]
             ;; Boxes and sequencing
             ['box
              (boxC (parse (list-ref tail 0)))]
             ['unbox
              (unboxC (parse (list-ref tail 0)))]
             ['set-box!
              (setboxC (parse (list-ref tail 0)) (parse (list-ref tail 1)))]
             ['seq
              (seqC (parse (list-ref tail 0)) (parse (list-ref tail 1)))]
             ;; Advice
             ['label
              (labelC (list-ref tail 0) (parse (list-ref tail 1)))]
             ['aroundapp
              (let ([defpair (car (list-ref tail 0))])
                (aroundAppC (list-ref defpair 0) (parse (list-ref defpair 1)) (parse (list-ref tail 1))))]
             ;; Input/Output
             ['read
              (readC (list-ref tail 0))]
             ['write
              (writeC (list-ref tail 0) (parse (list-ref tail 1)))]
             ['file
              (fileC (list-ref tail 0))]
             ;; Application
             [else
              (appC (parse head) (parse (list-ref tail 0)))]))]
        [(number? s) (numC s)]
        [(symbol? s) (idC s)] 
        [else (error 'parse "Unhandled case")]))
;)

(define (parse-file (path path-string?)) ExprC?
  (call-with-input-file path (lambda (port) (parse (read port)))))