#lang plai

(define (Any? x) true)

(define-type Value
  (numV (n number?))
  (closV (c Closure?))
  (boxV (l Location?))
  (namedV (name symbol?) (value Value?))
  )

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

(define-type Binding
  [bind (name symbol?) (loc Location?)])
(define Env? (curry andmap Binding?))

;; Variables and the store

(define (lookup [for symbol?] [env Env?]) Location?
  (cond
    [(empty? env) (error 'lookup (string-append "name not found: " (symbol->string for)))]
    [else 
     (type-case Binding (first env)
       [bind (name loc)
             (cond
               [(symbol=? for name) loc]
               [else (lookup for (rest env))])])]))
       
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

(define-type Closure
  [closureC (arg symbol?) (body ExprC?) (env Env?)])

(define-type Advice
  [aroundC (name symbol?) (closure Closure?)])
(define AdvEnv? (curry andmap Advice?))  

(define (extract-names [val Value?]) pair?
  (type-case Value val
    [namedV (name v)
            (let ([p (extract-names v)])
              '((cons name (car p)) (cdr p)))]
    [else '(empty val)]))

(define (get-advice [names (curry andmap symbol?)] [env AdvEnv?]) (curry andmap Closure?)
  (cond
    [(empty? env) empty]
    [(cons? env) 
     (let ([others (get-advice names (rest env))])
       (type-case Advice (first env)
         [aroundC (name closure)
                  (cond
                    [(member name names) (cons closure others)]
                    [else others])]))]))

(define-type ExprC
  ;; Numbers, arithmetic, and conditionals
  [numC (n number?)]
  [plusC (l ExprC?) (r ExprC?)]
  [multC (l ExprC?) (r ExprC?)]
  [ifZeroOrLessC (c ExprC?) (t ExprC?) (f ExprC?)]
  ;; Identifiers and functions
  [idC (s symbol?)]
  [appC (fun ExprC?) (arg ExprC?)]
  [lamC (arg symbol?) (body ExprC?)]
  [letC (s symbol?) (v ExprC?) (in ExprC?)]
  ;; Boxes and sequencing
  [boxC (arg ExprC?)]
  [unboxC (arg ExprC?)]
  [setboxC (b ExprC?) (v ExprC?)]
  [seqC (b1 ExprC?) (b2 ExprC?)]
  ;; Advice
  [proceedC (arg ExprC?)]
  ;; Input/Output
  [writeC (l string?) (v ExprC?)]
  [readC (l string?)]
)