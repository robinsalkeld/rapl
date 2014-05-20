#lang plai

(define (Any? x) true)

(define-type Value
  (numV (n number?)))

(define (num+ l r)
  (cond
    [(and (numV? l) (numV? r))
     (numV (+ (numV-n l) (numV-n r)))]
    [else
     (error 'num+ "one argument was not a number")]))

(define (num* l r) Value?
  (cond
    [(and (numV? l) (numV? r))
     (numV (* (numV-n l) (numV-n r)))]
    [else
     (error 'num* "one argument was not a number")]))

(define (numWrite (v Value?))
   (cond
    [(numV? v)
     (write (numV-n v))]
    [else
     (error 'numWrite "argument was not a number")]))

(define (list-box-push! b x)
  (set-box! b (cons x (unbox b))))
(define (list-box-pop! b)
  (let* ([next (first (unbox b))]
         [_ (set-box! b (rest (unbox b)))])
         next))

(define read-source (box (lambda () (string->number (read-line)))))

(define interp-input (box '()))
(define (record-interp-input (x number?))
  (set-box! interp-input (cons x (unbox interp-input))))
(define get-interp-input 
  (lambda () (reverse (unbox interp-input))))

(define-type DefC
  [bindC (name symbol?) (loc Location?)]
  [funC (name symbol?) (arg symbol?) (body ExprC?)]
  [aroundC (name symbol?) (arg symbol?) (body ExprC?)]
  [inlineC (name symbol?) (arg symbol?) (body ExprC?)])
(define Env? (curry andmap DefC?))

;; Variables and the store

(define (lookup [for symbol?] [env Env?]) Location?
  (cond
    [(empty? env) (error 'lookup (string-append "name not found: " (symbol->string for)))]
    [else 
     (type-case DefC (first env)
       [bindC (name loc)
             (cond
            [(symbol=? for name) loc]
            [else (lookup for (rest env))])]
       [else (lookup for (rest env))])]))
       
(define Location? number?)

(define-type Storage
  [cell (location Location?) (val Value?)])
 
(define (fetch [sto Store?] [loc Location?]) Value?
  (cond
    [(empty? sto) (error 'fetch "location not found")]
    [else (cond
            [(= loc (cell-location (first sto)))
             (cell-val (first sto))]
            [else (fetch (rest sto) loc)])]))

(define Store? (curry andmap Storage?))
  
(define (new-loc [sto Store?]) Location?
  (length sto))

(define (override-store [sto Store?] [loc Location?] [value Value?])
  (cons (cell loc value) sto))

(define mt-store empty)

(define-type Context
  [e*s (e Env?) (s Store?)])

(define (display-context [c Context?])
  (begin (display "Environment: \n")
         (map (lambda (def) (begin (display "\t") (display def) (display "\n"))) (e*s-e c))
         (display "Store: \n")
         (map (lambda (c) (begin (display "\t") (display c) (display "\n"))) (e*s-s c))))
         
(define-type Result
  [v*s (v Value?) (s Store?)])

;; Functions and advice

(define-type ClosureC
  [closureC (arg symbol?) (body ExprC?) (env Env?)])

(define (get-fundef [n symbol?] [env Env?]) ClosureC?
  (cond
    [(empty? env) (error 'get-fundef "reference to undefined function")]
    [(cons? env) 
     (type-case DefC (first env)
       [funC (name arg body)
             (cond
                   [(equal? n name) (closureC arg body env)]
                   [else (get-fundef n (rest env))])]
       [else (get-fundef n (rest env))])]))
   
(define (get-advice [n symbol?] [env Env?] [calling-env Env?]) (curry andmap ClosureC?)
  (cond
    [(empty? env) empty]
    [(cons? env) 
     (let ([others (get-advice n (rest env) calling-env)])
       (type-case DefC (first env)
         [aroundC (name arg body)
                  (cond
                    [(equal? n name) (cons (closureC arg body env) others)]
                    [else others])]
         [inlineC (name arg body)
                  (cond
                    [(equal? n name) (cons (closureC arg body calling-env) others)]
                    [else others])]
         [else others]))]))

(define-type ExprC
  [numC (n number?)]
  [varC (s symbol?)]
  [appC (fun symbol?) (arg ExprC?)]
  [plusC (l ExprC?) (r ExprC?)]
  [multC (l ExprC?) (r ExprC?)]
  [setC (s symbol?) (v ExprC?)]
  [letC (s symbol?) (v ExprC?) (in ExprC?)]
  [defC (d DefC?) (in ExprC?)]
  [seqC (b1 ExprC?) (b2 ExprC?)]
  [ifZeroOrLessC (c ExprC?) (t ExprC?) (f ExprC?)]
  [proceedC (v ExprC?)]
  [writeC (l string?) (v ExprC?)]
  [readC (l string?)]
)