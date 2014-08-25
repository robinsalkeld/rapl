#lang plai

(require "miraj.rkt")
(require "miraj_parser.rkt")
(require "miraj_serialization.rkt")

(define miraj-ns (module->namespace "miraj_interpreter_no_store.rkt"))

;;
;; Miraj interpreter
;;

(define-type Value
  (numV (n number?))
  (closV (arg symbol?) (body ExprC?) (env Env?))
  (namedV (name symbol?) (value Value?)))

;; Numbers, arithmetic, and conditionals

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

;; Identifiers and functions

(define-type Binding
  [bind (name symbol?) (value Value?)])
(define Env? (curry andmap Binding?))
(define mt-env empty)

(define (lookup [for symbol?] [env Env?]) Value?
  (cond
    [(empty? env) (error 'lookup (string-append "name not found: " (symbol->string for)))]
    [else 
     (type-case Binding (first env)
       [bind (name loc)
             (cond
               [(symbol=? for name) loc]
               [else (lookup for (rest env))])])]))

(define (interp-with-binding [name symbol?] [a Value?] [expr ExprC?] [env Env?] [adv AdvEnv?]) Value?
  (interp expr (cons (bind name a) env) adv))

(define (interp-closure-app [closure Value?] [a Value?] [adv AdvEnv?]) Value? 
  (type-case Value closure
    [closV (arg body env)
           (interp-with-binding arg a body env adv)]
    [else (error 'interp "only abstractions can be applied")]))

;; Advice

(define-type Advice
  [aroundAppV (name symbol?) (value Value?)])
(define AdvEnv? (curry andmap Advice?))  
(define mt-adv empty)

(define (apply-around-app [name symbol?] [adv AdvEnv?] [advice Advice?] [f Value?]) Value?
  (type-case Advice advice
      [aroundAppV (advised-name w)
                  (cond
                    [(symbol=? name advised-name)
                     (interp-closure-app w f adv)]
                    [else f])]))

(define (weave-app [name symbol?] [f Value?] [adv AdvEnv?]) Value?
  (foldr (curry apply-around-app name adv) f adv))

(define (display-with-label [label string?] [val Value?])
  (begin (display label) (display ": ") (numWrite val) (newline)))

(define (interp [expr ExprC?] [env Env?] [adv AdvEnv?]) Value?
;(begin (display "Expression: ") (display expr) (newline)
;       (display-context (e*s env sto)) (newline)
  (type-case ExprC expr
    
    ;; Numbers, arithmetic, and conditionals
    
    [numC (n) (numV n)]
    
    [plusC (l r) (let ([v-l (interp l env adv)])
                   (let ([v-r (interp r env adv)])
                      (num+ v-l v-r)))]

    [multC (l r) (let ([v-l (interp l env adv)])
                   (let ([v-r (interp r env adv)])
                      (num* v-l v-r)))]
    
    [ifZeroC (c t f) (let ([v-c (interp c env adv)])
                       (cond 
                         [(<= (numV-n v-c) 0) (interp t env adv)]
                         [else (interp f env adv)]))]
    
    ;; Identifiers and functions
    
    [idC (n) (lookup n env)]
    
    [lamC (a b) (closV a b env)]
    
    [appC (f a) (let ([v-f (interp f env adv)])
                  (let ([v-a (interp a env adv)])
                         (type-case Value v-f
                           [namedV (name labelled-f)
                                   (let ([v-w (weave-app name labelled-f adv)])
                                     (interp-closure-app v-w v-a adv))]
                           [else (interp-closure-app v-f v-a adv)])))]
    
    [letC (s val in) (let ([v-val (interp val env adv)])
                            (interp-with-binding s v-val in env adv))]
    
    ;; Advice
    
    [labelC (name v) (namedV name (interp v env adv))]
    
    [aroundAppC (name f in) (let ([v-f (interp f env adv)])
                  (interp in env (cons (aroundAppV name v-f) adv)))]
                
    ;; Input/Output
    
    [fileC (path) (interp (parse-file path) env adv)]
    
    [writeC (l a) (let ([v-a (interp a env adv)])
                    (begin (display-with-label l v-a) v-a))]
    
    [readC (l) (let* ([_ (display l)]
                      [_ (display "> ")]
                      [val ((unbox read-source))]
                      [_ (record-interp-input val)])
                 (numV val))]

    [seqC (b1 b2) (begin (interp b1 env adv)
                         (interp b2 env adv))]
    
    [else (error 'interp "Expression type not supported")]))
;)

(define (interp-exp [exp ExprC?]) Value?
  (interp exp mt-env mt-adv))

   