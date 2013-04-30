#lang plai-typed

;;
;; Miraj interpreter
;;

(define-type Value
  [numV (n : number)]
  [boxV (l : Location)]
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
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [boxC (arg : ExprC)]
  [unboxC (arg : ExprC)]
  [setboxC (b : ExprC) (v : ExprC)]
  [seqC (b1 : ExprC) (b2 : ExprC)]
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

(define-type Result
  [v*s (v : Value) (s : Store)])

(define (interp [expr : ExprC] [env : Env] [fds : (listof FunDefC)] [sto : Store]) : Result
  (type-case ExprC expr
    [numC (n) (v*s (numV n) sto)]
    [idC (n) (v*s (fetch (lookup n env) sto) sto)]
    [appC (f a) (local ([define fd (get-fundef f fds)])
           (type-case Result (interp a env fds sto)
                 [v*s (v-a s-a)
                      (let ([where (new-loc)])
                        (interp (fdC-body fd)
                                (extend-env (bind (fdC-arg fd) where) mt-env)
                                fds
                                (override-store (cell where v-a) s-a))
                      )
                 ]
           ))]
    
    [plusC (l r) (type-case Result (interp l env fds sto)
               [v*s (v-l s-l)
                    (type-case Result (interp r env fds s-l)
                      [v*s (v-r s-r)
                           (v*s (num+ v-l v-r) s-r)])])]

    [multC (l r) (type-case Result (interp l env fds sto)
               [v*s (v-l s-l)
                    (type-case Result (interp r env fds s-l)
                      [v*s (v-r s-r)
                           (v*s (num* v-l v-r) s-r)])])]
    
    [boxC (a) (type-case Result (interp a env fds sto)
            [v*s (v-a s-a)
                 (let ([where (new-loc)])
                   (v*s (boxV where)
                        (override-store (cell where v-a)
                                        s-a)))])]
    
    [unboxC (a) (type-case Result (interp a env fds sto)
              [v*s (v-a s-a)
                   (v*s (fetch (boxV-l v-a) s-a) s-a)])]

    [setboxC (b v) (type-case Result (interp b env fds sto)
                 [v*s (v-b s-b)
                      (type-case Result (interp v env fds s-b)
                        [v*s (v-v s-v)
                             (v*s v-v
                                  (override-store (cell (boxV-l v-b)
                                                        v-v)
                                                  s-v))])])]
    
    [seqC (b1 b2) (type-case Result (interp b1 env fds sto)
                [v*s (v-b1 s-b1)
                     (interp b2 env fds s-b1)])]
  )
)

(test (v*s-v (interp (plusC (numC 10) (appC 'const5 (numC 10)))
              mt-env
              (list (fdC 'const5 '_ (numC 5)))
              mt-store))
      (numV 15))
 
(test (v*s-v (interp (plusC (numC 10) (appC 'double (plusC (numC 1) (numC 2))))
              mt-env
              (list (fdC 'double 'x (plusC (idC 'x) (idC 'x))))
              mt-store))
      (numV 16))
 
(test (v*s-v (interp (plusC (numC 10) (appC 'quadruple (plusC (numC 1) (numC 2))))
              mt-env
              (list (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))
                    (fdC 'double 'x (plusC (idC 'x) (idC 'x))))
              mt-store))
      (numV 22))