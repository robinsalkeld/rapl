#lang plai

(require "miraj.rkt")
(require "miraj_parser.rkt")
(require "miraj_serialization.rkt")

;;
;; Miraj interpreter
;;

(define-type Value
  (numV (n number?))
  (closV (arg symbol?) (body ExprC?) (env Env?))
  (boxV (l Location?))
  (namedV (name symbol?) (value Value?))
  (builtinV (f procedure?)))

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
  [bind (name symbol?) (loc Location?)])
(define Location? number?)
(define Env? (curry andmap Binding?))
(define mt-env empty)

(define (lookup [for symbol?] [env Env?])
  (cond
    [(empty? env) (error 'lookup (string-append "name not found: " (symbol->string for)))]
    [else 
     (type-case Binding (first env)
       [bind (name loc)
             (cond
               [(symbol=? for name) loc]
               [else (lookup for (rest env))])])]))

(define (interp-with-binding [name symbol?] [a Value?] [expr ExprC?] [env Env?] [adv AdvEnv?] [sto Store?]) Result?
  (let* ([where (new-loc sto)]
         [new-env (cons (bind name where) env)]
         [new-sto (override-store sto where a)])
            (interp expr new-env adv new-sto)))

(define (interp-closure-app [closure Value?] [a Value?] [adv AdvEnv?] [sto Store?]) Result? 
  (type-case Value closure
    [closV (arg body env)
           (interp-with-binding arg a body env adv sto)]
    [builtinV (f)
             (f a adv sto)]
    [else (error 'interp "only abstractions can be applied")]))

(define-type JoinPoint
  [app-call (label symbol?) (abs Value?) (arg Value?) (adv AdvEnv?) (sto Store?)]
  [app-return (label symbol?) (abs Value?) (result Value?) (adv AdvEnv?) (sto Store?)])

(define interp-jps (box '()))
(define (record-interp-jp (jp JoinPoint?))
  (list-box-push! interp-jps jp))
(define get-interp-jps
  (lambda () (reverse (unbox interp-jps))))


;; Mutations and side-effects

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

(define-type Result
  [v*s (v Value?) (s Store?)])

(define-type Context
  [e*s (e Env?) (s Store?)])

;; Advice

(define-type Advice
  [aroundAppV (name symbol?) (value Value?)]
  [aroundSetV (name symbol?) (value Value?)])
(define AdvEnv? (curry andmap Advice?))  
(define mt-adv empty)

(define (apply-around-app [name symbol?] [adv AdvEnv?] [advice Advice?] [abs-sto Result?]) Result?
  (type-case Advice advice
      [aroundAppV (advised-name f)
                  (cond
                    [(symbol=? name advised-name)
                     (type-case Result abs-sto
                       (v*s (abs sto) 
                            (interp-closure-app f abs adv sto)))]
                    [else abs-sto])]
      [else abs-sto]))

(define (weave-app [name symbol?] [adv AdvEnv?] [f Value?] [sto Store?]) Result?
  (foldr (curry apply-around-app name adv) (v*s f sto) adv))

;; Debugging

(define (display-value [v Value?])
  (type-case Value v
    [numV (n) (display n)]
    [closV (arg body env)
           (begin (display arg) (display " -> ") (display (exp-syntax body)) (display-env env))]
    [boxV (l)
          (begin (display "box(") (display l) (display ")"))]
    [namedV (l a)
            (begin (display "(label ") (display l) (display " ") (display-value a) (display ")"))]
    [builtinV (f) (display f)]))

(define (display-context [c Context?])
  (begin (display "Environment: \n")
         (display-env (e*s-e c))
         (display "Store: \n")
         (display-store (e*s-s c))))
         
(define (display-env [env Env?])
  (map (lambda (def) 
         (type-case Binding def
           [bind (n l)
                 (begin (display "\t") (display n) (display " -> ") (display l) (display "\n"))])) 
       env))

(define (display-store [sto Store?])
  (map (lambda (c) 
         (type-case Storage c
           [cell (l v)
                 (begin (display "\t") (display l) (display " -> ") (display-value v) (display "\n"))])) 
       sto))

(define (display-with-label [label string?] [val Value?])
  (begin (display label) (display ": ") (numWrite val) (newline)))

(define (interp [expr ExprC?] [env Env?] [adv AdvEnv?] [sto Store?]) Result?
(begin (display "Expression: ") (display (exp-syntax expr)) (newline)
       (display-context (e*s env sto)) (newline)
  (type-case ExprC expr
    
    ;; Numbers, arithmetic, and conditionals
    
    [numC (n) (v*s (numV n) sto)]
    
    [plusC (l r) (type-case Result (interp l env adv sto)
               [v*s (v-l s-l)
                    (type-case Result (interp r env adv s-l)
                      [v*s (v-r s-r)
                           (v*s (num+ v-l v-r) s-r)])])]

    [multC (l r) (type-case Result (interp l env adv sto)
               [v*s (v-l s-l)
                    (type-case Result (interp r env adv s-l)
                      [v*s (v-r s-r)
                           (v*s (num* v-l v-r) s-r)])])]
    
    [ifZeroC (c t f) (type-case Result (interp c env adv sto)
               [v*s (v-c s-c)
                    (cond 
                       [(<= (numV-n v-c) 0) (interp t env adv s-c)]
                       [else (interp f env adv s-c)])])]
    
    ;; Identifiers and abstractions
    
    [idC (n) (v*s (fetch sto (lookup n env)) sto)]
    
    [lamC (a b) (v*s (closV a b env) sto)]
    
    [appC (f a) (type-case Result (interp f env adv sto)
                  [v*s (v-f s-f)
                       (type-case Result (interp a env adv s-f)
                         [v*s (v-a s-a) 
                              (type-case Value v-f
                                [namedV (name labelled-f)
                                        (type-case Result (weave-app name adv labelled-f s-a)
                                          [v*s (v-w s-w)
                                               (let* ([_ (record-interp-jp (app-call name v-w v-a adv s-w))]
                                                      [result (interp-closure-app v-w v-a adv s-w)]
                                                      [_ (record-interp-jp (app-return name v-w v-a adv s-w))])
                                                 result)])]
                                [else (interp-closure-app v-f v-a adv s-a)])])])]
    
    [letC (s val in) (type-case Result (interp val env adv sto)
                            [v*s (v-val s-val)
                                 (interp-with-binding s v-val in env adv s-val)])]
    
    ;; Boxes and sequencing
    
    [boxC (a) (type-case Result (interp a env adv sto)
                [v*s (v-a s-a)
                     (let ([where (new-loc sto)])
                       (v*s (boxV where)
                            (override-store s-a where v-a)))])]
    
    [unboxC (a) (type-case Result (interp a env adv sto)
              [v*s (v-a s-a)
                   (v*s (fetch s-a (boxV-l v-a)) s-a)])]
    
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
    
    [labelC (name v) 
            (type-case Result (interp v env adv sto)
              [v*s (v-v s-v)
                   (v*s (namedV name v-v) sto)])]
    
    [aroundAppC (name f in) 
                (type-case Result (interp f env adv sto)
                  [v*s (v-f s-f)
                       (interp in env (cons (aroundAppV name v-f) adv) s-f)])]
                
    
    [aroundSetC (name f in) 
                (type-case Result (interp f env adv sto)
                  [v*s (v-f s-f)
                       (interp in env (cons (aroundSetV name v-f) adv) s-f)])]
    
    ;; Input/Output
    
    [fileC (path) (interp (parse-file path) mt-env adv sto)]
    
    [writeC (l a) (type-case Result (interp a env adv sto)
               [v*s (v-a s-a) (begin (display-with-label l v-a) (v*s v-a s-a))])]
    
    [readC (l) (let* ([_ (display l)]
                      [_ (display "> ")]
                      [val ((unbox read-source))]
                      [_ (record-interp-input val)])
                 (v*s (numV val) sto))]))
)

(define (interp-exp [exp ExprC?]) Value?
  (v*s-v (interp exp mt-env mt-adv mt-store)))
   
(define (app-chain [exps list?]) ExprC?
  (foldl (lambda (next chained) (appC chained next)) (first exps) (rest exps)))