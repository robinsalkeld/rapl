#lang plai

(require "miraj.rkt")

;;
;; Miraj interpreter
;;

(define-type Env
  [envV (vars VarEnv?) (fds FunEnv?) (ads AdvEnv?)])
(define mt-env (envV empty empty empty))

(define (with-defs [vars VarDefs?] [fds FunDefs?] [ads AdvDefs?] [env Env?] [sto Store?] [cc procedure?]) 
  (with-vars vars env sto (lambda (env sto)
                            (let* ([define-func (lambda (f) (funV (fdC-name f) (fdC-arg f) (fdC-body f) (box mt-env)))]
                                   [define-adv (lambda (a) (aroundV (aroundC-name a) (aroundC-arg a) (aroundC-body a) (box mt-env)))]
                                   [new-env (envV (envV-vars env) 
                                                  (append (envV-fds env) (map define-func fds)) 
                                                  (append (envV-ads env) (map define-adv ads)))]
                                   [_ (map (lambda (f) (set-box! (funV-env f) new-env)) (envV-fds new-env))]
                                   [_ (map (lambda (a) (set-box! (aroundV-env a) new-env)) (envV-ads new-env))])
                              (cc new-env sto)))))

(define (with-vars [vars VarDefs?] [env Env?] [sto Store?] [cc procedure?])
  (cond
    [(empty? vars) 
     (cc env sto)]
    [else
     (let ([var (first vars)])
       (with-var (vdC-name var) (vdC-value var) env sto 
                 (lambda (new-env new-sto)
                   (with-vars (rest vars) new-env new-sto))))]))

(define (with-var [name symbol?] [a Value?] [env Env?] [sto Store?] [cc procedure?]) 
  (type-case Env env
    [envV (vars fds ads)
          (let ([where (new-loc)])
            (cc (envV (cons (bind name where) vars) fds ads) (override-store (cell where a) sto)))]))

(define (interp-with-binding [name symbol?] [a Value?] [expr ExprC?] [env Env?] [sto Store?] [proceed procedure?]) Result?
  (with-var name a env sto (lambda (new-env new-sto) (interp expr new-env new-sto proceed))))

(define (apply-advice [n symbol?] [advice AdviceV?] [proceed procedure?]) procedure?
  (type-case AdviceV advice
      [aroundV (name param body env)
               (cond
                 [(symbol=? n name) 
                  (lambda (val sto) 
                    (interp-with-binding param val body (unbox env) sto proceed))]
                 [else proceed])]))

(define (no-proceed (val Value?) (sto Store?)) Result?
  (error 'no-proceed "proceed called outside of advice"))

(define (weave [n symbol?] [ads AdvEnv?] [proceed procedure?]) procedure?
  (foldr (lambda (advice p) (apply-advice n advice p)) proceed ads))

(define (call-closure [fd FunV?]) procedure? 
  (type-case FunV fd
    [funV (name arg body env)
          (lambda (val sto)
            (let* ([_ (record-interp-jp (call name val sto))]
                   [result (interp-with-binding arg val body (unbox env) sto no-proceed)]
                   [_ (record-interp-jp (return name result))])
                  result))]))

(define (interp [expr ExprC?] [env Env?] [sto Store?] [proceed procedure?]) Result?
  (type-case ExprC expr
    [numC (n) (v*s (numV n) sto)]
    [varC (n) (v*s (fetch (lookup n (envV-vars env)) sto) sto)]
    [appC (f a) (type-case Result (interp a env sto proceed)
               [v*s (v-a s-a) 
                    (let* ([fd (get-fundef f (envV-fds env))]
                           [cc (call-closure fd)]
                           [woven-closure (weave f (envV-ads env) cc)])
                      (woven-closure v-a s-a))])]
    
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
                         (v*s v-v (override-store (cell where v-v) s-v)))])]
    
    [letC (s val in) (type-case Result (interp val env sto proceed)
               [v*s (v-val s-val)
                    (interp-with-binding s v-val in env s-val proceed)])]
    
    [seqC (b1 b2) (type-case Result (interp b1 env sto proceed)
               [v*s (v-b1 s-b1)
                    (interp b2 env s-b1 proceed)])]
    
    [ifZeroOrLessC (c t f) (type-case Result (interp c env sto proceed)
               [v*s (v-c s-c)
                    (cond 
                       [(<= (numV-n v-c) 0) (interp t env s-c proceed)]
                       [else (interp f env s-c proceed)])])]
    
    [proceedC (a) (type-case Result (interp a env sto proceed)
               [v*s (v-a s-a) (proceed v-a s-a)])]
    
    [writeC (a) (type-case Result (interp a env sto proceed)
               [v*s (v-a s-a) (begin (numWrite v-a) (display "\n") (v*s v-a s-a))])]
    
    [readC () (let ([val ((unbox read-source))]) 
               (begin (record-interp-input val) (v*s (numV val) sto)))]
  )
)

(define-type MirajProgram
  [miraj (vars VarDefs?) (fds FunDefs?) (ads AdvDefs?) (exp ExprC?)]
)

(define mt-program (miraj empty empty empty (numC 0)))

(define (append-programs [p1 MirajProgram?] [p2 MirajProgram?])
  ;; TODO-RS: Should verify that the environments are mutatually exclusive
  (miraj (append (miraj-vars p1) (miraj-vars p2))
         (append (miraj-fds p1) (miraj-fds p2))
         (append (miraj-ads p1) (miraj-ads p2))
         (seqC (miraj-exp p1) (miraj-exp p2))))

(define (interp-program [mp MirajProgram?])
  (type-case MirajProgram mp
    [miraj (vars fds ads exp)
           (with-defs vars fds ads mt-env mt-store (lambda (env sto)
                                                    (interp exp env sto no-proceed)))]))

(define-type MirajRecording
  [mirajRecForReplay (program MirajProgram?) (input list?)])

(define-type MirajTrace
  [mirajTrace (program MirajProgram?) (joinpoints list?)])
