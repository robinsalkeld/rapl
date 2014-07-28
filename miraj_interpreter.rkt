#lang plai

(require "miraj.rkt")

;;
;; Miraj interpreter
;;

(define mt-env empty)

(define (interp-with-binding [name symbol?] [a Value?] [expr ExprC?] [env Env?] [sto Store?] [proceed procedure?]) Result?
  (let* ([where (new-loc sto)]
         [new-env (cons (bind name a) env)]
         [new-sto (override-store sto where a)])
            (interp expr new-env new-sto proceed)))

(define (apply-advice [advice Closure?] [proceed procedure?]) procedure?
  (type-case Closure advice
      [closureC (param body adv-env)
                (lambda (val env sto) 
                  (interp-with-binding param val body adv-env sto proceed))]))

(define (no-proceed [val Value?] [env Env?] [sto Store?]) Result?
  (error 'no-proceed "proceed called outside of advice"))

(define (weave [names (curry andmap symbol?)] [env Env?] [proceed procedure?]) procedure?
  (foldr (lambda (advice p) (apply-advice advice p)) proceed (get-advice names env)))

(define-type JoinPoint
  [call (names (curry andmap symbol?)) (a Value?) (c Context?)]
  [return (names (curry andmap symbol?)) (result Value?) (c Context?)])

(define interp-jps (box '()))
(define (record-interp-jp (jp JoinPoint?))
  (set-box! interp-jps (cons jp (unbox interp-jps))))
(define get-interp-jps
  (lambda () (reverse (unbox interp-jps))))

(define (call-closure [names (curry andmap symbol?)] [closure Closure?]) procedure? 
  (type-case Closure closure
    [closureC (arg body fun-env)
              (lambda (val env sto)
                (let* ([_ (record-interp-jp (call names val (e*s env sto)))]
                       [result (interp-with-binding arg val body fun-env sto no-proceed)]
                       [_ (record-interp-jp (return names (v*s-v result) (e*s env (v*s-s result))))])
                  result))]))

(define (display-with-label [label string?] [val Value?])
  (begin (display label) (display ": ") (numWrite val) (newline)))

(define (interp [expr ExprC?] [env Env?] [sto Store?] [proceed procedure?]) Result?
;(begin (display "Expression: ") (display expr) (newline)
;       (display-context (e*s env sto)) (newline)
  (type-case ExprC expr
    
    ;; Numbers, arithmetic, and conditionals
    
    [numC (n) (v*s (numV n) sto)]
    
    [plusC (l r) (type-case Result (interp l env sto proceed)
               [v*s (v-l s-l)
                    (type-case Result (interp r env s-l proceed)
                      [v*s (v-r s-r)
                           (v*s (num+ v-l v-r) s-r)])])]

    [multC (l r) (type-case Result (interp l env sto proceed)
               [v*s (v-l s-l)
                    (type-case Result (interp r env s-l proceed)
                      [v*s (v-r s-r)
                           (v*s (num* v-l v-r) s-r)])])]
    
    [ifZeroOrLessC (c t f) (type-case Result (interp c env sto proceed)
               [v*s (v-c s-c)
                    (cond 
                       [(<= (numV-n v-c) 0) (interp t env s-c proceed)]
                       [else (interp f env s-c proceed)])])]
    
    ;; Identifiers and functions
    
    [idC (n) (v*s (fetch sto (lookup n env)) sto)]
    
    [appC (f a) (type-case Result (interp f env sto)
                  [v*s (v-f s-f)
                       (type-case Result (interp a env s-f proceed)
                         [v*s (v-a s-a) 
                              (let* ([names*closure (extract-names v-f)]
                                     [cc (call-closure (cdr names*closure))]
                                     [woven-closure (weave (car names*closure) env cc)])
                                (woven-closure v-a env s-a))])])]
    
    [letC (s val in) (type-case Result (interp val env sto proceed)
                            [v*s (v-val s-val)
                                 (interp-with-binding s v-val in env s-val proceed)])]
    
    [lamC (a b) (closV (closureC a b env))]
    
    ;; Boxes and sequencing
    
    [boxC (a) (type-case Result (interp a env sto proceed)
                [v*s (v-a s-a)
                     (let ([where (new-loc sto)])
                       (v*s (boxV where)
                            (override-store (cell where v-a)
                                            s-a)))])]
    
    [unboxC (a) (type-case Result (interp a env sto proceed)
              [v*s (v-a s-a)
                   (v*s (fetch (boxV-l v-a) s-a) s-a)])]
    
    [setboxC (b val) (type-case Result (interp b env sto proceed)
                       [v*s (v-b s-b)
                            (type-case Result (interp val env s-b proceed)
                              [v*s (v-v s-v)
                                   (let ([where (boxV-l v-b)])
                                     (v*s v-v (override-store s-v where v-v)))])])]
    
    [seqC (b1 b2) (type-case Result (interp b1 env sto proceed)
               [v*s (v-b1 s-b1)
                    (interp b2 env s-b1 proceed)])]
    
    ;; Advice
    
    [proceedC (a) (type-case Result (interp a env sto proceed)
               [v*s (v-a s-a) (proceed v-a env s-a)])]
    
    ;; Input/Output
    
    [writeC (l a) (type-case Result (interp a env sto proceed)
               [v*s (v-a s-a) (begin (display-with-label l v-a) (v*s v-a s-a))])]
    
    [readC (l) (let* ([_ (display l)]
                      [_ (display "> ")]
                      [val ((unbox read-source))]
                      [_ (record-interp-input val)])
                 (v*s (numV val) sto))]))
;)

(define (chain-interp [exps list?] [base-proceed procedure?]) Result?
  (let* ([fold-fn (lambda (exp proceed) (lambda (val env sto) (interp exp env sto proceed)))])
    ((foldr fold-fn base-proceed exps) (numV 0) mt-env mt-store)))

(define (interp-program [exps list?])
  (chain-interp exps no-proceed))

