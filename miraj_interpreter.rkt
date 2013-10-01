#lang plai

;;
;; Miraj interpreter
;;

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

(define read-source (box (lambda () (string->number (read-line)))))

(define interp-input (box '()))
(define (record-interp-input (x number?))
  (set-box! interp-input (cons x (unbox interp-input))))
(define get-interp-input 
  (lambda () (reverse (unbox interp-input))))

(define-type JoinPoint
  [call (name symbol?) (a Value?)]
  [return (name symbol?) (result Value?)]
  )

(define interp-jps (box '()))
(define (record-interp-jps (jp JoinPoint?))
  (set-box! interp-jps (cons jp (unbox interp-jps))))
(define get-interp-jps
  (lambda () (reverse (unbox interp-jps))))

(define Location? number?)

(define new-loc
  (let ([n (box 0)])
    (lambda ()
      (begin
        (set-box! n (add1 (unbox n)))
        (unbox n)))))

(define-type Binding
  [bind (name symbol?) (val Location?)])
 
(define Env? (curry andmap Binding?))
(define mt-env empty)
(define extend-env cons)

(define-type Storage
  [cell (location Location?) (val Value?)])
 
(define Store? (curry andmap Storage?))
(define mt-store empty)
(define override-store cons)

(define (lookup [for symbol?] [env Env?]) Location?
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

(define-type FunDefC
  [fdC (name symbol?) (arg symbol?) (body ExprC?)])
(define FunEnv? (curry andmap FunDefC?))

(define (get-fundef [n symbol?] [fds FunEnv?]) FunDefC?
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(cons? fds) (cond
                   [(equal? n (fdC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))

(define-type AdviceDefC
  [aroundC (name symbol?) (arg symbol?) (body ExprC?)])
(define no-advice empty)
(define AdvEnv? (curry andmap AdviceDefC?))

(define (apply-advice [n symbol?] [fds FunEnv?] [ads AdvEnv?] [advice AdviceDefC?] [proceed procedure?]) procedure?
  (type-case AdviceDefC advice
      [aroundC (name param body)
               (cond
                 [(symbol=? n name) 
                  (lambda (val sto) (interp-with-binding body param val mt-env fds ads sto proceed))]
                 [else proceed])]))

(define-type Result
  [v*s (v Value?) (s Store?)])

(define (interp-with-binding [expr ExprC?] [name symbol?] [a Value?] [env Env?] [fds FunEnv?] [ads AdvEnv?] [sto Store?] [proceed procedure?]) Result?
  (let ([where (new-loc)])
  (interp expr
          (extend-env (bind name where) env)
          fds
          ads
          (override-store (cell where a) sto)
          proceed)
))

(define (no-proceed (val Value?) (sto Store?)) Result?
  (error 'no-proceed "proceed called outside of advice"))

(define (weave [n symbol?] [fds FunEnv?] [ads AdvEnv?] [proceed procedure?]) procedure?
  (foldr (lambda (val sto) (apply-advice n fds ads val sto)) proceed ads))

(define (interp [expr ExprC?] [env Env?] [fds FunEnv?] [ads AdvEnv?] [sto Store?] [proceed procedure?]) Result?
  (type-case ExprC expr
    [numC (n) (v*s (numV n) sto)]
    [varC (n) (v*s (fetch (lookup n env) sto) sto)]
    [appC (f a) (type-case Result (interp a env fds ads sto proceed)
               [v*s (v-a s-a) 
                    (local ([define fd (get-fundef f fds)]
                            [define call-closure (lambda (closure-val closure-sto) (interp-with-binding (fdC-body fd) (fdC-arg fd) closure-val mt-env fds ads closure-sto no-proceed))]
                            [define woven-closure (weave f fds ads call-closure)]) 
                           (woven-closure v-a s-a))])]
    
    [plusC (l r) (type-case Result (interp l env fds ads sto proceed)
               [v*s (v-l s-l)
                    (type-case Result (interp r env fds ads s-l proceed)
                      [v*s (v-r s-r)
                           (v*s (num+ v-l v-r) s-r)])])]

    [multC (l r) (type-case Result (interp l env fds ads sto proceed)
               [v*s (v-l s-l)
                    (type-case Result (interp r env fds ads s-l proceed)
                      [v*s (v-r s-r)
                           (v*s (num* v-l v-r) s-r)])])]
    
    [setC (var val) (type-case Result (interp val env fds ads sto proceed)
                        [v*s (v-v s-v)
                             (let ([where (lookup var env)])
                               (v*s v-v (override-store (cell where v-v) s-v))
                             )
                        ]
                      )]
    
    [letC (s val in) (type-case Result (interp val env fds ads sto proceed)
                [v*s (v-val s-val)
                     (interp-with-binding in s v-val env fds ads s-val proceed)])]
    
    [seqC (b1 b2) (type-case Result (interp b1 env fds ads sto proceed)
                [v*s (v-b1 s-b1)
                     (interp b2 env fds ads s-b1 proceed)])]
    
    [ifZeroOrLessC (c t f) (type-case Result (interp c env fds ads sto proceed)
                [v*s (v-c s-c)
                     (cond 
                       [(<= (numV-n v-c) 0) (interp t env fds ads s-c proceed)]
                       [else (interp f env fds ads s-c proceed)])])]
    
    [proceedC (a) (type-case Result (interp a env fds ads sto proceed)
               [v*s (v-a s-a) (proceed v-a s-a)])]
    
    [writeC (a) (type-case Result (interp a env fds ads sto proceed)
               [v*s (v-a s-a) (begin (numWrite v-a) (display "\n") (v*s v-a s-a))])]
    
    [readC () (let ([val ((unbox read-source))]) 
                (begin (record-interp-input val) (v*s (numV val) sto)))]
  )
)

(define-type MirajProgram
  [miraj (fds FunEnv?) (ads AdvEnv?) (exp ExprC?)]
)

(define (interp-program [mp MirajProgram?])
  (interp (miraj-exp mp) mt-env (miraj-fds mp) (miraj-ads mp) mt-store no-proceed)
)

(define-type MirajRecording
  [mirajRecForReplay (program MirajProgram?) (input list?)])

