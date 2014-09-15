#lang plai

(require "miraj.rkt")
(require "miraj_parser.rkt")
(require "miraj_serialization.rkt")

;;
;; Miraj interpreter
;;

(define-type Value
  (numV (n number?))
  (boolV (b boolean?))
  (strV (s string?))
  (closV (arg symbol?) (body ExprC?) (env Env?))
  (boxV (l Location?))
  (taggedV (tag Value?) (value Value?))
  (builtinV (f procedure?)))

;; Numbers and arithmetic

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

;; Booleans and conditionals

(define (deep-untag [v Value?])
  (type-case Value v
    [taggedV (tag tagged)
             (deep-untag tagged)]
    [else v]))

(define (equal-values [l Value?] [r Value?]) Value?
  (let ([r-untagged (deep-untag r)])
    (boolV
     (type-case Value (deep-untag l)
       [boolV (lb)
              (type-case Value r-untagged [boolV (rb) (equal? lb rb)] [else false])]
       [numV (ln)
             (type-case Value r-untagged [numV (rn) (equal? ln rn)] [else false])]
       [strV (ls)
             (type-case Value r-untagged [strV (rs) (equal? ls rs)] [else false])]
       [boxV (ll)
             (type-case Value r-untagged [boxV (rl) (equal? ll rl)] [else false])]
       [else false]))))
     

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
  (type-case Value (deep-untag closure)
    [closV (arg body env)
           (interp-with-binding arg a body env adv sto)]
    [builtinV (f)
             (f a adv sto)]
    [else (error 'interp "only abstractions can be applied")]))

(define-type JoinPoint
  [app-call (abs Value?) (arg Value?) (adv AdvEnv?) (sto Store?)]
  [app-return (abs Value?) (result Value?) (adv AdvEnv?) (sto Store?)])

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
  [aroundAppV (value Value?)]
  [aroundSetV (value Value?)])
(define AdvEnv? (curry andmap Advice?))  
(define mt-adv empty)

(define (apply-around-app [adv AdvEnv?] [advice Advice?] [abs-sto Result?]) Result?
  (type-case Advice advice
    [aroundAppV (f)
                (type-case Result abs-sto
                  (v*s (abs sto) 
                       (interp-closure-app f abs adv sto)))]
    [else abs-sto]))

(define (weave [adv AdvEnv?] [f Value?] [sto Store?]) Result?
  (foldr (curry apply-around-app adv) (v*s f sto) adv))

(define (interp-app [abs Value?] [arg Value?] [adv AdvEnv?] [sto Store?]) Result?
  (let* ([_ (record-interp-jp (app-call abs arg adv sto))]
         [woven-abs-result (weave adv abs sto)]
         [result (interp-closure-app (v*s-v woven-abs-result) arg adv (v*s-s woven-abs-result))]
         [_ (record-interp-jp (app-return abs (v*s-v result) adv (v*s-s result)))])
    result))

;; Debugging

(define (display-value [v Value?])
  (type-case Value v
    [numV (n) (display n)]
    [boolV (b) (display b)]
    [strV (s) (write s)]
    [closV (arg body env)
           (begin (display arg) (display " -> ") (display (exp-syntax body)) (display-env env))]
    [boxV (l)
          (begin (display "box(") (display l) (display ")"))]
    [taggedV (t v)
            (begin (display "(tag ") (display-value t) (display " ") (display-value v) (display ")"))]
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
  (begin (display label) (display ": ") (display-value val) (newline)))

(define verbose-interp (box false))

(define (interp [expr ExprC?] [env Env?] [adv AdvEnv?] [sto Store?]) Result?
(begin 
  (if (unbox verbose-interp)
      (begin
        (display "Expression: ") (display (exp-syntax expr)) (newline)
        (display-context (e*s env sto)) (newline))
      '())

  (type-case ExprC expr
    
    ;; Numbers and arithmetic
    
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
    
    ;; Booleans and conditionals
    
    [boolC (b) (v*s (boolV b) sto)]
    
    [equalC (l r) (type-case Result (interp l env adv sto)
               [v*s (v-l s-l)
                    (type-case Result (interp r env adv s-l)
                      [v*s (v-r s-r)
                           (v*s (equal-values v-l v-r) s-r)])])]
    
    [ifC (c t f) (type-case Result (interp c env adv sto)
                   [v*s (v-c s-c)
                        (if (boolV-b v-c)
                            (interp t env adv s-c)
                            (interp f env adv s-c))])]
    
    ;; Identifiers and abstractions
    
    [idC (n) (v*s (fetch sto (lookup n env)) sto)]
    
    [lamC (a b) (v*s (closV a b env) sto)]
    
    [appC (f a) (type-case Result (interp f env adv sto)
                  [v*s (v-f s-f)
                       (type-case Result (interp a env adv s-f)
                         [v*s (v-a s-a) 
                              (interp-app v-f v-a adv s-a)])])]
    
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
    
    [strC (s) (v*s (strV s) sto)]
    
    [tagC (tag v) 
          (type-case Result (interp tag env adv sto)
            [v*s (v-tag s-tag)
                 (type-case Result (interp v env adv s-tag)
                   [v*s (v-v s-v)
                        (v*s (taggedV v-tag v-v) s-tag)])])]
    
    [tagtestC (v f g)
              (type-case Result (interp v env adv sto)
                  [v*s (v-v s-v)
                       (type-case Value v-v
                         [taggedV (tag tagged)
                                  (type-case Result (interp f env adv s-v)
                                    [v*s (v-f s-f)
                                         (type-case Result (interp-app v-f tag adv s-f)
                                           [v*s (v-f2 s-f2)
                                                (interp-app v-f2 tagged adv s-f2)])])]
                         [else (interp g env adv s-v)])])]
                                         
                         
    [aroundAppC (f in) 
                (type-case Result (interp f env adv sto)
                  [v*s (v-f s-f)
                       (interp in env (cons (aroundAppV v-f) adv) s-f)])]
                
    
    [aroundSetC (f in) 
                (type-case Result (interp f env adv sto)
                  [v*s (v-f s-f)
                       (interp in env (cons (aroundSetV v-f) adv) s-f)])]
    
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