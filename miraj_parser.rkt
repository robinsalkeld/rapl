#lang plai

(require "miraj.rkt")

(define (parse s) ExprC?
  (cond [(list? s) 
         (let ([head (car s)]
               [tail (cdr s)])
           (case head
             ['+
              (plusC (parse (list-ref tail 0)) (parse (list-ref tail 1)))]
             ['*
              (multC (parse (list-ref tail 0)) (parse (list-ref tail 1)))]
             ['lambda
              (lamC (list-ref tail 0) (parse (list-ref tail 1)))]
             ['let
              (letC (list-ref tail 0) (parse (list-ref tail 1)) (parse (list-ref tail 2)))]
             ['aroundapp
              (aroundAppC (list-ref tail 0) (parse (list-ref tail 1)) (parse (list-ref tail 2)))]
             [else
              (appC (parse head) (parse (list-ref tail 0)))]))]
        [(number? s) (numC s)]
        [(symbol? s) (idC s)] 
        [else (error 'parse "Unhandled case")]))