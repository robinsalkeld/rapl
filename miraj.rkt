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
  [aroundC (name symbol?) (arg symbol?) (body ExprC?)])
(define Env? (curry andmap DefC?))

;; Variables and the store

(define (lookup [for symbol?] [env Env?]) Location?
  (cond
    [(empty? env) (error 'lookup "name not found")]
    [else 
     (type-case DefC (first env)
       [bindC (name loc)
             (cond
            [(symbol=? for name) loc]
            [else (lookup for (rest env))])]
       [else (lookup for (rest env))])]))
       
(define (Location? x) true)

(define-type Storage
  [cell (location Location?) (val Value?)])
 
(define (fetch-cell [loc Location?] [cells (curry andmap Storage?)]) Value?
  (cond
    [(empty? cells) (error 'fetch-cell "location not found")]
    [else (cond
            [(= loc (cell-location (first cells)))
             (cell-val (first cells))]
            [else (fetch-cell loc (rest cells))])]))

(define-type Store 
  [store (newer procedure?) (getter procedure?) (setter procedure?) (serializer procedure?) (loader procedure?)])

(define (new-loc [sto Store?]) Location?
  ((store-newer sto)))

(define (fetch [sto Store?] [loc Location?]) Value?
  ((store-getter sto) loc))

(define (override-store [sto Store?] [loc Location?] [value Value?])
  ((store-setter sto) loc value))

(define (serialize-store [sto Store?])
  ((store-serializer sto)))
  
(define (deserialize-store [sto Store?] [data struct?])
  ((store-loader sto) data))

(define (list-store [cells (curry andmap Storage?)])
  (store (lambda () (length cells))
         (lambda (loc) (fetch-cell loc cells))
         (lambda (loc value) (list-store (cons (cell loc value) cells)))
         (lambda () cells)
         (lambda (cs) (list-store cs))))
  
(define mt-store (list-store empty))

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
   
(define (get-advice [n symbol?] [env Env?]) (curry andmap ClosureC?)
  (cond
    [(empty? env) empty]
    [(cons? env) 
     (type-case DefC (first env)
       [aroundC (name arg body)
                (cond
                   [(equal? n name) (cons (closureC arg body env) (get-advice n (rest env)))]
                   [else (get-advice n (rest env))])]
       [else (get-advice n (rest env))])]))

(define-type ExprC
  [numC (n number?)]
  [varC (s symbol?)]
  [appC (fun symbol?) (arg ExprC?)]
  [plusC (l ExprC?) (r ExprC?)]
  [multC (l ExprC?) (r ExprC?)]
  [setC (s symbol?) (v ExprC?)]
  [letC (d DefC?) (in ExprC?)]
  [seqC (b1 ExprC?) (b2 ExprC?)]
  [ifZeroOrLessC (c ExprC?) (t ExprC?) (f ExprC?)]
  [proceedC (v ExprC?)]
  [writeC (v ExprC?)]
  [readC]
)