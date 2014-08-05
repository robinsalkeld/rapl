#lang plai

(require "miraj.rkt")
(require "miraj_interpreter.rkt")
;(require "miraj_serialization.rkt")
;(require "miraj_recording.rkt")

;(test (v*s-v (interp-exp 
;              (letC 'const5 (lamC '_ (numC 5)) 
;                    (plusC (numC 10) (appC (idC 'const5) (numC 10))))))
;      (numV 15))
; 
;(test (v*s-v (interp-exp 
;              (letC 'double (lamC 'x (plusC (idC 'x) (idC 'x)))
;                    (plusC (numC 10) (appC (idC 'double) (plusC (numC 1) (numC 2)))))))
;      (numV 16))
; 
;(test (v*s-v (interp-exp 
;              (letC 'double (lamC 'x (plusC (idC 'x) (idC 'x)))
;                    (letC 'quadruple (lamC 'x (appC (idC 'double) (appC (idC 'double) (idC 'x))))
;                          (plusC (numC 10) (appC (idC 'quadruple) (plusC (numC 1) (numC 2))))))))
;      (numV 22))
;
;(test (v*s-v (interp-exp
;              (aroundAppC 'change 
;                          (lamC 'proceed (lamC 'y 
;                                               (appC (idC 'proceed) (multC (idC 'y) (numC 2)))))
;                       (aroundAppC 'change 
;                                   (lamC 'proceed (lamC 'y 
;                                                        (appC (idC 'proceed) (plusC (idC 'y) (numC 3)))))
;                                   (letC 'change (labelC 'change (lamC 'x (plusC (idC 'x) (numC 5))))
;                                         (appC (idC 'change) (numC 2)))))))
;      (numV 15))
;
;(test (v*s-v (interp-exp
;              (aroundAppC 'change 
;                          (lamC 'proceed (lamC 'y 
;                                               (appC (idC 'proceed) (plusC (idC 'y) (numC 3)))))
;                          (aroundAppC 'change 
;                                      (lamC 'proceed (lamC 'y 
;                                                           (appC (idC 'proceed) (multC (idC 'y) (numC 2)))))
;                       
;                                      (letC 'change (labelC 'change (lamC 'x (plusC (idC 'x) (numC 5))))
;                                         (appC (idC 'change) (numC 2)))))))
;      (numV 12))
;
;(test (v*s-v (interp-exp (writeC "The answer" (numC 42))))
;      (numV 42))

(define y-comb
  ; \f -> (\x -> f (\y -> (x x) y) (\x -> f (\y -> (x x) y)
  (lamC 'f (appC (lamC 'x (appC (idC 'f) (lamC 'y (appC (appC (idC 'x) (idC 'x)) (idC 'y))))) 
                 (lamC 'x (appC (idC 'f) (lamC 'y (appC (appC (idC 'x) (idC 'x)) (idC 'y))))))))

(define fact-rec 
  (lamC 'fact (labelC 'fact 
        (lamC 'x 
              (ifZeroOrLessC (idC 'x) 
                             (numC 1) 
                             (multC (idC 'x) (appC (idC 'fact) (plusC (idC 'x) (numC -1)))))))))

;(test (v*s-v (interp-exp (appC (appC y-comb fact-rec) (numC 4))))
;      (numV 24))

(test (v*s-v (interp-exp 
                         (aroundAppC 'fact (lamC 'proceed 
                                                 (lamC 'y (letC 'result (appC (idC 'proceed) (idC 'y))
                                                                (seqC (writeC "y" (idC 'y))
                                                                      (seqC (writeC "result" (idC 'result))
                                                                            (idC 'result))))))
                                     (writeC "fact(3)" (appC (appC y-comb fact-rec) (numC 3))))))
      (numV 6))

;(test (struct->list/recursive (numC 4)) '(numC 4))
;(test (struct->list/recursive (plusC (numC 3) (numC 4))) '(plusC (numC 3) (numC 4)))
;(test (struct->list/recursive (appC 'foo (numC 4))) '(appC 'foo (numC 4)))
;
;(define (test-roundtrip e) (test (list->struct/recursive miraj-ns (struct->list/recursive e)) e))
;
;(test-roundtrip (plusC (numC 3) (numC 4)))
;(test-roundtrip (appC 'foo (numC 4)))
;(test-roundtrip (funC 'foo 'bar (multC (numC 4) (idC 'bar))))