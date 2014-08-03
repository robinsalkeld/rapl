#lang plai

(require "miraj.rkt")

;;
;; Miraj interpreter
;;

(define mt-env empty)

(define (interp-with-binding [name symbol?] [a Value?] [expr ExprC?] [env Env?] [adv AdvEnv?] [sto Store?]) Result?
  (let* ([where (new-loc sto)]
         [new-env (cons (bind name where) env)]
         [new-sto (override-store sto where a)])
            (interp expr new-env adv new-sto)))

(define (interp-closure-app [closure Value?] [a Value?] [adv AdvEnv?] [sto Store?]) Result? 
  (type-case Value closure
    [closV (arg body env)
           (interp-with-binding arg a body env adv sto)]
    [else (error 'interp "only abstractions can be applied")]))

(define (apply-around-app [name symbol?] [adv AdvEnv?] [advice Advice?] [r Result?]) Result?
  (type-case Advice advice
      [aroundAppV (advised-name f)
                  (cond
                    [(symbol=? name advised-name)
                     (type-case Result r
                       (v*s (abs sto) 
                            (interp-closure-app f abs adv sto)))]
                    [else r])]
      [else r]))

(define (weave-app [name symbol?] [adv AdvEnv?] [f Value?] [sto Store?]) Result?
  (foldr (curry apply-around-app name adv) (v*s f sto) adv))

(define (display-with-label [label string?] [val Value?])
  (begin (display label) (display ": ") (numWrite val) (newline)))

(define (interp [expr ExprC?] [env Env?] [adv AdvEnv?] [sto Store?] [proceed procedure?]) Result?
(begin (display "Expression: ") (display expr) (newline)
       (display-context (e*s env sto)) (newline)
  (type-case ExprC expr
    
    ;; Numbers, arithmetic, and conditionals
    
    [numC (n) (v*s (numV n) sto)]
    
    [plusC (l r) (type-case Result (interp l env adv sto proceed)
               [v*s (v-l s-l)
                    (type-case Result (interp r env adv s-l proceed)
                      [v*s (v-r s-r)
                           (v*s (num+ v-l v-r) s-r)])])]

    [multC (l r) (type-case Result (interp l env adv sto proceed)
               [v*s (v-l s-l)
                    (type-case Result (interp r env adv s-l proceed)
                      [v*s (v-r s-r)
                           (v*s (num* v-l v-r) s-r)])])]
    
    [ifZeroOrLessC (c t f) (type-case Result (interp c env adv sto proceed)
               [v*s (v-c s-c)
                    (cond 
                       [(<= (numV-n v-c) 0) (interp t env adv s-c proceed)]
                       [else (interp f env adv s-c proceed)])])]
    
    ;; Identifiers and functions
    
    [idC (n) (v*s (fetch sto (lookup n env)) sto)]
    
    [appC (f a) (type-case Result (interp f env adv sto)
                  [v*s (v-f s-f)
                       (type-case Result (interp a env adv s-f)
                         [v*s (v-a s-a) 
                              (type-case Value v-f
                                [closV (arg body f-env)
                                       (interp-with-binding arg v-a body f-env adv s-a)]
                                [namedV (name subf)
                                        (type-case Result (weave-app name adv subf s-a)
                                          [v*s (v-w s-w)
                                               (interp-closure-app v-w v-a adv s-w)])]
                                [else (error 'interp "only abstractions can be applied")])])])]
    
    [letC (s val in) (type-case Result (interp val env adv sto)
                            [v*s (v-val s-val)
                                 (interp-with-binding s v-val in env adv s-val)])]
    
    [lamC (a b) (v*s (closV a b env) sto)]
    
    ;; Boxes and sequencing
    
    [boxC (a) (type-case Result (interp a env adv sto)
                [v*s (v-a s-a)
                     (let ([where (new-loc sto)])
                       (v*s (boxV where)
                            (override-store (cell where v-a) s-a)))])]
    
    [unboxC (a) (type-case Result (interp a env adv sto)
              [v*s (v-a s-a)
                   (v*s (fetch (boxV-l v-a) s-a) s-a)])]
    
    [setboxC (b val) (type-case Result (interp b env adv sto)
                       [v*s (v-b s-b)
                            (type-case Result (interp val env adv s-b)
                              [v*s (v-v s-v)
                                   (let ([where (boxV-l v-b)])
                                     (v*s v-v (override-store s-v where v-v)))])])]
    
    [seqC (b1 b2) (type-case Result (interp b1 env adv sto)
               [v*s (v-b1 s-b1)
                    (interp b2 env adv s-b1)])]
    
    ;; Advice
    
    [labelC (name v) (type-case Result (interp v env adv sto)
               [v*s (v-v s-v)
                    (v*s (namedV name v-v) sto)])]
    
    [aroundAppC (name f in) (type-case Result (interp f env adv sto)
                  [v*s (v-f s-f)
                       (interp in env (cons (aroundAppV name v-f) adv) s-f)])]
                
    
    [aroundSetC (name f in) (type-case Result (interp f env adv sto)
                  [v*s (v-f s-f)
                       (interp in env (cons (aroundSetV name v-f) adv) s-f)])]
    
    ;; Input/Output
    
    [writeC (l a) (type-case Result (interp a env adv sto)
               [v*s (v-a s-a) (begin (display-with-label l v-a) (v*s v-a s-a))])]
    
    [readC (l) (let* ([_ (display l)]
                      [_ (display "> ")]
                      [val ((unbox read-source))]
                      [_ (record-interp-input val)])
                 (v*s (numV val) sto))]))
)

(define (interp-exp [exp ExprC?])
  (interp exp mt-env mt-adv mt-store))

(define (interp-program [exps list?])
  (interp-exp (foldl (lambda (next chained) (appC chained next)) (first exps) (rest exps))))
   