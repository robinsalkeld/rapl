#lang plai-typed

;;
;; Miraj interpreter
;;

(define-type Value
  [numV (n : number)]
)

(define (num+ [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (+ (numV-n l) (numV-n r)))]
    [else
     (error 'num+ "one argument was not a number")]))

(define (num* [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (* (numV-n l) (numV-n r)))]
    [else
     (error 'num* "one argument was not a number")]))


(define-type ExprC
  [numC (n : number)]
  [varC (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [setC (s : symbol) (v : ExprC)]
  [seqC (b1 : ExprC) (b2 : ExprC)]
  [proceedC (v : ExprC)]
)

(define-type-alias Location number)

(define new-loc
  (let ([n (box 0)])
    (lambda ()
      (begin
        (set-box! n (add1 (unbox n)))
        (unbox n)))))

(define-type Binding
  [bind (name : symbol) (val : Location)])
 
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define-type Storage
  [cell (location : Location) (val : Value)])
 
(define-type-alias Store (listof Storage))
(define mt-store empty)
(define override-store cons)

(define (lookup [for : symbol] [env : Env]) : Location
  (cond
    [(empty? env) (error 'lookup "name not found")]
    [else (cond
            [(symbol=? for (bind-name (first env)))
             (bind-val (first env))]
            [else (lookup for (rest env))])]))

(define (fetch [loc : Location] [sto : Store]) : Value
  (cond
    [(empty? sto) (error 'fetch "location not found")]
    [else (cond
            [(= loc (cell-location (first sto)))
             (cell-val (first sto))]
            [else (fetch loc (rest sto))])]))

(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(cons? fds) (cond
                   [(equal? n (fdC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))

(define-type AdviceDefC
  [aroundC (name : symbol) (arg : symbol) (body : ExprC)])
(define no-advice empty)

(define (apply-advice [n : symbol] [fds : (listof FunDefC)] [ads : (listof AdviceDefC)] [advice : AdviceDefC] [proceed : (Value Store -> Result)]) : (Value Store -> Result)
  (type-case AdviceDefC advice
      [aroundC (name param body)
               (cond
                 [(symbol=? n name) (lambda (val sto) (interp-with-binding body param val fds ads sto proceed))]
                 [else proceed])]))

(define-type Result
  [v*s (v : Value) (s : Store)])

(define (interp-with-binding [expr : ExprC] [name : symbol] [a : Value] [fds : (listof FunDefC)] [ads : (listof AdviceDefC)] [sto : Store] [proceed : (Value Store -> Result)]) : Result
  (let ([where (new-loc)])
  (interp expr
          (extend-env (bind name where) mt-env)
          fds
          ads
          (override-store (cell where a) sto)
          proceed)
))

(define (no-proceed (val : Value) (sto : Store)) : Result
  (error 'no-proceed "proceed called outside of advice"))

(define (weave [n : symbol] [fds : (listof FunDefC)] [ads : (listof AdviceDefC)] [proceed : (Value Store -> Result)]) : (Value Store -> Result)
  (foldr (lambda (val sto) (apply-advice n fds ads val sto)) proceed ads))


  
(define (interp [expr : ExprC] [env : Env] [fds : (listof FunDefC)] [ads : (listof AdviceDefC)] [sto : Store] [proceed : (Value Store -> Result)]) : Result
  (type-case ExprC expr
    [numC (n) (v*s (numV n) sto)]
    [varC (n) (v*s (fetch (lookup n env) sto) sto)]
    [appC (f a) (type-case Result (interp a env fds ads sto proceed)
               [v*s (v-a s-a) 
                    (local ([define fd (get-fundef f fds)]
                            [define call-closure (lambda (closure-val closure-sto) (interp-with-binding (fdC-body fd) (fdC-arg fd) closure-val fds ads closure-sto no-proceed))]
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
    
    [seqC (b1 b2) (type-case Result (interp b1 env fds ads sto proceed)
                [v*s (v-b1 s-b1)
                     (interp b2 env fds ads s-b1 proceed)])]
    
    [proceedC (a) (type-case Result (interp a env fds ads sto proceed)
               [v*s (v-a s-a) (proceed v-a s-a)])]
  )
)

(test (v*s-v (interp (plusC (numC 10) (appC 'const5 (numC 10)))
              mt-env
              (list (fdC 'const5 '_ (numC 5)))
              no-advice
              mt-store
              no-proceed))
      (numV 15))
 
(test (v*s-v (interp (plusC (numC 10) (appC 'double (plusC (numC 1) (numC 2))))
              mt-env
              (list (fdC 'double 'x (plusC (varC 'x) (varC 'x))))
              no-advice
              mt-store
              no-proceed))
      (numV 16))
 
(test (v*s-v (interp (plusC (numC 10) (appC 'quadruple (plusC (numC 1) (numC 2))))
              mt-env
              (list (fdC 'quadruple 'x (appC 'double (appC 'double (varC 'x))))
                    (fdC 'double 'x (plusC (varC 'x) (varC 'x))))
              no-advice
              mt-store
              no-proceed))
      (numV 22))

(test (v*s-v (interp (plusC (numC 10) (appC 'quadruple (plusC (numC 1) (numC 2))))
              mt-env
              (list (fdC 'quadruple 'x (appC 'double (appC 'double (varC 'x))))
                    (fdC 'double 'x (seqC (setC 'x (plusC (varC 'x) (varC 'x))) (varC 'x))))
              no-advice
              mt-store
              no-proceed))
      (numV 22))

(test (v*s-v (interp (appC 'change (numC 2))
              mt-env
              (list (fdC 'change 'x (plusC (varC 'x) (numC 5))))
              (list (aroundC 'change 'y (proceedC (multC (varC 'y) (numC 2))))
                    (aroundC 'change 'y (proceedC (plusC (varC 'y) (numC 3)))))
              mt-store
              no-proceed))
      (numV 12))

(test (v*s-v (interp (appC 'change (numC 2))
              mt-env
              (list (fdC 'change 'x (plusC (varC 'x) (numC 5))))
              (list (aroundC 'change 'y (proceedC (plusC (varC 'y) (numC 3))))
                    (aroundC 'change 'y (proceedC (multC (varC 'y) (numC 2)))))
              mt-store
              no-proceed))
      (numV 15))

(test/exn (v*s-v (interp (appC 'foo (numC 2))
              mt-env
              (list (fdC 'foo 'x (proceedC (varC 'x))))
              empty
              mt-store
              no-proceed))
      "proceed called outside of advice")