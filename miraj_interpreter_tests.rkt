#lang plai

(require "miraj.rkt")
(require "miraj_interpreter.rkt")
(require "miraj_serialization.rkt")
;(require "miraj_recording.rkt")

(test (interp-exp 
              (letC 'const5 (lamC '_ (numC 5)) 
                    (plusC (numC 10) (appC (idC 'const5) (numC 10)))))
      (numV 15))
 
(test (interp-exp 
              (letC 'double (lamC 'x (plusC (idC 'x) (idC 'x)))
                    (plusC (numC 10) (appC (idC 'double) (plusC (numC 1) (numC 2))))))
      (numV 16))
 
(test (interp-exp 
              (letC 'double (lamC 'x (plusC (idC 'x) (idC 'x)))
                    (letC 'quadruple (lamC 'x (appC (idC 'double) (appC (idC 'double) (idC 'x))))
                          (plusC (numC 10) (appC (idC 'quadruple) (plusC (numC 1) (numC 2)))))))
      (numV 22))

(test (interp-exp
              (aroundAppC 'change 
                          (lamC 'proceed (lamC 'y 
                                               (appC (idC 'proceed) (multC (idC 'y) (numC 2)))))
                       (aroundAppC 'change 
                                   (lamC 'proceed (lamC 'y 
                                                        (appC (idC 'proceed) (plusC (idC 'y) (numC 3)))))
                                   (letC 'change (labelC 'change (lamC 'x (plusC (idC 'x) (numC 5))))
                                         (appC (idC 'change) (numC 2))))))
      (numV 15))

(test (interp-exp
              (aroundAppC 'change 
                          (lamC 'proceed (lamC 'y 
                                               (appC (idC 'proceed) (plusC (idC 'y) (numC 3)))))
                          (aroundAppC 'change 
                                      (lamC 'proceed (lamC 'y 
                                                           (appC (idC 'proceed) (multC (idC 'y) (numC 2)))))
                       
                                      (letC 'change (labelC 'change (lamC 'x (plusC (idC 'x) (numC 5))))
                                         (appC (idC 'change) (numC 2))))))
      (numV 12))

(test (interp-exp (writeC "The answer" (numC 42)))
      (numV 42))


(test (interp-exp (appC (fileC "fact.rkt") (numC 4)))
      (numV 24))

(test (interp-exp (appC (appC (fileC "fact_advice.rkt") (fileC "fact.rkt")) (numC 3)))
      (numV 6))

(test (struct->list/recursive (numC 4)) '(numC 4))
(test (struct->list/recursive (plusC (numC 3) (numC 4))) '(plusC (numC 3) (numC 4)))
(test (struct->list/recursive (appC (idC 'foo) (numC 4))) '(appC (idC 'foo) (numC 4)))

(define (test-roundtrip e) (test (list->struct/recursive miraj-ns (struct->list/recursive e)) e))

(test-roundtrip (plusC (numC 3) (numC 4)))
(test-roundtrip (appC (idC 'foo) (numC 4)))
(test-roundtrip (lamC 'bar (multC (numC 4) (idC 'bar))))