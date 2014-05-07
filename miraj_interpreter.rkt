#lang plai

(require "miraj.rkt")

;;
;; Miraj interpreter
;;

(define-type Env
  [envV (vars VarEnv?) (fds FunEnv?) (ads AdvEnv?)])
(define mt-env (envV empty empty empty))

(define (interp-with-binding [name symbol?] [a Value?] [expr ExprC?] [env Env?] [sto Store?] [proceed procedure?]) Result?
  (type-case Env env
    [envV (vars fds ads)
          (let* ([where (new-loc sto)]
                 [new-env (envV (cons (bind name where) vars) fds ads)]
                 [new-sto (override-store sto where a)])
            (interp expr new-env new-sto proceed))]))

(define (apply-advice [n symbol?] [advice AdviceV?] [proceed procedure?]) procedure?
  (type-case AdviceV advice
      [aroundV (name param body advice-env)
               (cond
                 [(symbol=? n name) 
                  (lambda (val env sto) 
                    (interp-with-binding param val body (unbox advice-env) sto proceed))]
                 [else proceed])]))

(define (no-proceed [val Value?] [env Env?] [sto Store?]) Result?
  (error 'no-proceed "proceed called outside of advice"))

(define (weave [n symbol?] [ads AdvEnv?] [proceed procedure?]) procedure?
  (foldr (lambda (advice p) (apply-advice n advice p)) proceed ads))

(define (call-closure [fd FunV?]) procedure? 
  (type-case FunV fd
    [funV (name arg body f-env)
          (lambda (val env sto)
            (let* ([_ (record-interp-jp (call name val))]
                   [result (interp-with-binding arg val body (unbox f-env) sto no-proceed)]
                   [_ (record-interp-jp (return name (v*s-v result)))])
                  result))]))

(define (interp [expr ExprC?] [env Env?] [sto Store?] [proceed procedure?]) Result?
;  (begin (write "interp: ") (write expr) (newline)
  (type-case ExprC expr
    [numC (n) (v*s (numV n) sto)]
    [varC (n) (v*s (fetch sto (lookup n (envV-vars env))) sto)]
    [appC (f a) (type-case Result (interp a env sto proceed)
               [v*s (v-a s-a) 
                    (let* ([fd (get-fundef f (envV-fds env))]
                           [cc (call-closure fd)]
                           [woven-closure (weave f (envV-ads env) cc)])
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
    
    [letC (s val in) (type-case Result (interp val env sto proceed)
               [v*s (v-val s-val)
                    (interp-with-binding s v-val in env s-val proceed)])]
    
    [funC (f in) (type-case FunDefC f
               [fdC (name arg body)
                    (let* ([fun (funV name arg body (box env))]
                           [new-env (envV (envV-vars env) 
                                          (cons fun (envV-fds env)) 
                                          (envV-ads env))]
                           [_ (set-box! (funV-env fun) new-env)])
                      (interp in new-env sto proceed))])]
    
    [advC (a in) (type-case AdviceDefC a
               [aroundC (name arg body)
                    (let* ([adv (aroundV name arg body (box env))]
                           [new-env (envV (envV-vars env) 
                                          (envV-fds env)
                                          (cons adv (envV-ads env)))]
                           [_ (set-box! (aroundV-env adv) new-env)])
                      (interp in new-env sto proceed))])]
    
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
    
    [writeC (a) (type-case Result (interp a env sto proceed)
               [v*s (v-a s-a) (begin (numWrite v-a) (newline) (v*s v-a s-a))])]
    
    [readC () (let ([val ((unbox read-source))]) 
               (begin (record-interp-input val) (v*s (numV val) sto)))]
  )
)
;)

(define (chain-interp [exps list?] [base-proceed procedure?]) Result?
  (let* ([fold-fn (lambda (exp proceed) (lambda (val env sto) (interp exp env sto proceed)))])
    ((foldr fold-fn base-proceed exps) (numV 0) mt-env mt-store)))

(define (interp-program [exps list?])
  (chain-interp exps no-proceed))

(define-type MirajRecording
  [mirajRecForReplay (program list?) (input list?)])

(define-type MirajTrace
  [mirajTrace (program list?) (joinpoints list?)])
