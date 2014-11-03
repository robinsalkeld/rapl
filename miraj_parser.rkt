#lang plai

(require "miraj.rkt")

(define (apply-parsed-args [p procedure?] [l list?])
  (cond [(equal? (procedure-arity p) (length l)) (apply p (map parse l))]
        [else (error 'parse "Wrong number of arguments")]))

(define (parse s) ExprC?
;  (begin (print s) (newline)
  (cond [(list? s) 
         (let ([head (car s)]
               [tail (cdr s)])
           (case head
             ;; Numbers and arithmetic
             ['+
              (apply-parsed-args plusC tail)]
             ['*
              (apply-parsed-args multC tail)]
             ;; Booleans and conditionals
             ['equal?
              (apply-parsed-args equalC tail)]
             ['if
              (apply-parsed-args ifC tail)]
             ;; Identifiers and functions
             ['lambda
              (let ([args (list-ref tail 0)])
                (foldr lamC (parse (list-ref tail 1)) args))]
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
             ['tag
              (tagC (parse (list-ref tail 0)) (parse (list-ref tail 1)))]
             ['tagtest
              (tagtestC (parse (list-ref tail 0)) (parse (list-ref tail 1)) (parse (list-ref tail 2)))]
             ['onapp
              (onappC (parse (list-ref tail 0)) (parse (list-ref tail 1)))]
             ;; Input/Output
             ['read
              (readC (list-ref tail 0))]
             ['write
              (writeC (list-ref tail 0) (parse (list-ref tail 1)))]
             ['file
              (fileC (list-ref tail 0))]
             ;; Application
             [else
              (let ([parsed (map parse s)])
                (foldl (lambda (x y) (appC y x)) (car parsed) (cdr parsed)))]))]
        [(number? s) (numC s)]
        [(string? s) (strC s)]
        [(boolean? s) (boolC s)]
        [(symbol? s) (idC s)] 
        [else (error 'parse "Unhandled case")]))
;)

(define (exp-syntax [e ExprC?])
  (type-case ExprC e
    [numC (n) n]
    [boolC (b) b]
    [strC (s) (string-append "\"" s "\"")]
    [plusC (l r) (list '+ (exp-syntax l) (exp-syntax r))]
    [multC (l r) (list '* (exp-syntax l) (exp-syntax r))]
    [equalC (l r) (list 'equal? (exp-syntax l) (exp-syntax r))]
    [ifC (c t f) (list 'if (exp-syntax c) (exp-syntax t) (exp-syntax f))]
    [idC (s) s]
    [lamC (a b) (list 'lambda (list a) (exp-syntax b))]
    [appC (f a) (list (exp-syntax f) (exp-syntax a))]
    [letC (s val in) (list 'let (list (list s (exp-syntax val))) (exp-syntax in))]
    [boxC (a) (list 'box (exp-syntax a))]
    [unboxC (a) (list 'unbox (exp-syntax a))]
    [setboxC (b val) (list 'set-box! (exp-syntax b) (exp-syntax val))]
    [seqC (b1 b2) (list 'seq (exp-syntax b1) (exp-syntax b2))]
    [tagC (tag v) (list 'tag (exp-syntax tag) (exp-syntax v))]
    [tagtestC (v f g) (list 'tagtest (exp-syntax v) (exp-syntax f) (exp-syntax g))]
    [onappC (f in) (list 'onapp (exp-syntax f) (exp-syntax in))]
    [aroundSetC (f in) (list 'aroundset (exp-syntax f) (exp-syntax in))]
    [fileC (path) (list 'file path)]
    [writeC (l a) (list 'write l (exp-syntax a))]
    [readC (l) (list 'read l)]))
    
(define (parse-file (path path-string?)) ExprC?
  (call-with-input-file path (lambda (port) (parse (read port)))))