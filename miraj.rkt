#lang plai

(define-type Value
  (numV (n number?))
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

(define-type ExprC
  [numC (n number?)]
  [varC (s symbol?)]
  [appC (fun symbol?) (arg ExprC?)]
  [plusC (l ExprC?) (r ExprC?)]
  [multC (l ExprC?) (r ExprC?)]
  [setC (s symbol?) (v ExprC?)]
  [letC (s symbol?) (val ExprC?) (in ExprC?)]
  [seqC (b1 ExprC?) (b2 ExprC?)]
  [ifZeroOrLessC (c ExprC?) (t ExprC?) (f ExprC?)]
  [proceedC (v ExprC?)]
  [writeC (v ExprC?)]
  [readC]
)

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

(define-type JoinPoint
  [call (name symbol?) (a Value?) (sto Store?)]
  [return (name symbol?) (result Result?)])

(define interp-jps (box '()))
(define (record-interp-jp (jp JoinPoint?))
  (set-box! interp-jps (cons jp (unbox interp-jps))))
(define get-interp-jps
  (lambda () (reverse (unbox interp-jps))))

;; Variables and the store

(define Location? number?)

(define new-loc
  (let ([n (box 0)])
    (lambda ()
      (begin
        (set-box! n (add1 (unbox n)))
        (unbox n)))))

(define-type VarDefC
  [vdC (name symbol?) (value ExprC?)])
(define VarDefs? (curry andmap VarDefC?))

(define-type Binding
  [bind (name symbol?) (val Location?)])
(define VarEnv? (curry andmap Binding?))

(define-type Storage
  [cell (location Location?) (val Value?)])
 
(define Store? (curry andmap Storage?))
(define mt-store empty)
(define override-store cons)

(define (lookup [for symbol?] [env VarEnv?]) Location?
  (cond
    [(empty? env) (error 'lookup "name not found")]
    [else (cond
            [(symbol=? for (bind-name (first env)))
             (bind-val (first env))]
            [else (lookup for (rest env))])]))

(define (fetch [loc Location?] [sto Store?]) Value?
  (cond
    [(empty? sto) (error 'fetch "location not found")]
    [else (cond
            [(= loc (cell-location (first sto)))
             (cell-val (first sto))]
            [else (fetch loc (rest sto))])]))

(define-type Result
  [v*s (v Value?) (s Store?)])

;; Functions and advice

(define-type FunDefC
  [fdC (name symbol?) (arg symbol?) (body ExprC?)])
(define FunDefs? (curry andmap FunDefC?))
(define-type FunV
  [funV (name symbol?) (arg symbol?) (body ExprC?) (env box?)])
(define FunEnv? (curry andmap FunV?))

(define-type AdviceDefC
  [aroundC (name symbol?) (arg symbol?) (body ExprC?)])
(define AdvDefs? (curry andmap AdviceDefC?))
(define-type AdviceV
  [aroundV (name symbol?) (arg symbol?) (body ExprC?) (env box?)])
(define AdvEnv? (curry andmap AdviceV?))

(define (get-fundef [n symbol?] [fds FunEnv?]) FunV?
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(cons? fds) (cond
                   [(equal? n (funV-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))

