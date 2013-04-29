#lang plai

(require rackunit)

;;
;; ba.rkt - A language of Boolean and Arithmetic Expressions
;;



;; t in Term
;; t ::= (true-t) | (false-t) | (if-then-else t t t) 
;;     | (z) | (succ t) | (pred t) | (zero? t)
(define-type term
 [true-t]
 [false-t]
 [if-then-else (pred term?) (conseq term?) (altern term?)]
 [z]
 [succ (arg term?)]
 [pred (arg term?)]
 [zero-t? (arg term?)])

;; nv in Num
;; nv ::= (z) | (succ nv)
(define (num? t)
 (cond
   [(z? t) true]
   [(succ? t) (num? (succ-arg t))]
   [else false]))

;; Test cases
(check-equal? (num? (z)) true)
(check-equal? (num? (succ (z))) true)
(check-equal? (num? (succ (true-t))) false)


;; v in Value
;; v ::= (true-t) | (false-t) | nv
(define (value? t)
 (or (true-t? t) (false-t? t) (num? t)))

(check-equal? (value? (true-t)) true)
(check-equal? (value? (pred (z))) false)


;; t ⇓ v (the big-step relation)
(define (big-step t)
 (type-case term t
   [true-t () (true-t)]
   [false-t () (false-t)]
   [if-then-else (pred conseq altern)
     (let ([v (big-step pred)])
       (cond
         [(true-t? pred) (big-step conseq)]
         [(false-t? pred) (big-step altern)]
         [else 
          (error "if-then-else expects a boolean in predicate position.")]))]
   [z () (z)]
   [succ (arg)
     (let ([v (big-step arg)])
       (if (num? v)
           (succ v)
           (error "succ expects a numeral.")))]
   [pred (arg)
     (let ([v (big-step arg)])
       (if (and (num? v) (succ? v))
           (succ-arg v)
           (error "pred expects a non-zero numeral.")))]
   [zero-t? (arg)
     (let ([v (big-step arg)])
       (cond
         [(z? v) (true-t)]
         [(num? v) (false-t)]
         [else (error "zero? expects a numeral")]))]))


(define pgm 
 (if-then-else (false-t)
   (z) 
   (if-then-else (true-t)
     (succ (z))
     (succ (succ (z))))))

(check-equal? (big-step (true-t)) (true-t))
(check-equal? (big-step pgm)
             (succ (z)))


;; nat : num -> Natural
(define (nat nv)
 (if (z? nv) 
     0
     (add1 (nat (succ-arg nv)))))

(check-equal? (nat (z)) 0)
(check-equal? (nat (succ (z))) 1)


;; eval-ba : term -> Boolean or Natural
(define (eval-ba t)
 (let ([v (big-step t)])
   (cond
     [(true-t? v) true]
     [(false-t? v) false]
     [else (nat v)])))

(check-equal? (eval-ba (true-t)) true)
(check-equal? (eval-ba pgm) 1)