#lang plai

(require "miraj.rkt")

;;
;; Miraj interpreter
;;

(define mt-env empty)

(define (interp-with-binding [name symbol?] [a Value?] [expr ExprC?] [env Env?] [sto Store?] [proceed procedure?]) Result?
  (let* ([where (new-loc sto)]
         [new-env (cons (bindC name where) env)]
         [new-sto (override-store sto where a)])
            (interp expr new-env new-sto proceed)))

(define (apply-advice [advice ClosureC?] [proceed procedure?]) procedure?
  (type-case ClosureC advice
      [closureC (param body adv-env)
                (lambda (val env sto) 
                  (interp-with-binding param val body adv-env sto proceed))]))

(define (no-proceed [val Value?] [env Env?] [sto Store?]) Result?
  (error 'no-proceed "proceed called outside of advice"))

(define (weave [n symbol?] [env Env?] [proceed procedure?]) procedure?
  (foldr (lambda (advice p) (apply-advice advice p)) proceed (get-advice n env env)))

(define-type JoinPoint
  [call (name symbol?) (a Value?) (c Context?)]
  [return (name symbol?) (result Value?) (c Context?)])

(define interp-jps (box '()))
(define (record-interp-jp (jp JoinPoint?))
  (set-box! interp-jps (cons jp (unbox interp-jps))))
(define get-interp-jps
  (lambda () (reverse (unbox interp-jps))))

(define (call-closure [name symbol?] [closure ClosureC?]) procedure? 
  (type-case ClosureC closure
    [closureC (arg body fun-env)
              (lambda (val env sto)
                (let* ([_ (record-interp-jp (call name val (e*s env sto)))]
                       [result (interp-with-binding arg val body fun-env sto no-proceed)]
                       [_ (record-interp-jp (return name (v*s-v result) (e*s env (v*s-s result))))])
                  result))]))

(define (display-with-label [label string?] [val Value?])
  (begin (display label) (display ": ") (numWrite val) (newline)))

(define (interp [expr ExprC?] [env Env?] [sto Store?] [proceed procedure?]) Result?
;(begin (display "Expression: ") (display expr) (newline)
;       (display-context (e*s env sto)) (newline)
  (type-case ExprC expr
    [numC (n) (v*s (numV n) sto)]
    [varC (n) (v*s (fetch sto (lookup n env)) sto)]
    [appC (f a) (type-case Result (interp a env sto proceed)
               [v*s (v-a s-a) 
                    (let* ([fd (get-fundef f env)]
                           [cc (call-closure f fd)]
                           [woven-closure (weave f env cc)])
                      (woven-closure v-a env s-a))])]
    
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
    
    [setC (var val) (type-case Result (interp val env sto proceed)
               [v*s (v-v s-v)
                    (let ([where (lookup var env)])
                         (v*s v-v (override-store s-v where v-v)))])]
    
    [letVarC (s val in) (type-case Result (interp val env sto proceed)
                            [v*s (v-val s-val)
                                 (interp-with-binding s v-val in env s-val proceed)])]
    
    [letFunC (s arg body in) (interp in (cons (funC s arg body) env) sto proceed)]
    
    [letAroundC (s arg body in) (interp in (cons (aroundC s arg body) env) sto proceed)]
    
    [letInlineC (s arg body in) (interp in (cons (inlineC s arg body) env) sto proceed)]
    
    [seqC (b1 b2) (type-case Result (interp b1 env sto proceed)
               [v*s (v-b1 s-b1)
                    (interp b2 env s-b1 proceed)])]
    
    [ifZeroOrLessC (c t f) (type-case Result (interp c env sto proceed)
               [v*s (v-c s-c)
                    (cond 
                       [(<= (numV-n v-c) 0) (interp t env s-c proceed)]
                       [else (interp f env s-c proceed)])])]
    
    [proceedC (a) (type-case Result (interp a env sto proceed)
               [v*s (v-a s-a) (proceed v-a env s-a)])]
    
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

