#lang plai

(require "miraj.rkt")
(require "miraj_interpreter.rkt")
(require "miraj_serialization.rkt")
(require "miraj_recording.rkt")

(test (v*s-v (interp (plusC (numC 10) (appC 'const5 (numC 10)))
              (list (funC 'const5 '_ (numC 5)))
              mt-store
              no-proceed))
      (numV 15))
 
(test (v*s-v (interp (plusC (numC 10) (appC 'double (plusC (numC 1) (numC 2))))
              (list (funC 'double 'x (plusC (varC 'x) (varC 'x))))
              mt-store
              no-proceed))
      (numV 16))
 
(test (v*s-v (interp (plusC (numC 10) (appC 'quadruple (plusC (numC 1) (numC 2))))
              (list (funC 'quadruple 'x (appC 'double (appC 'double (varC 'x))))
                    (funC 'double 'x (plusC (varC 'x) (varC 'x))))
              mt-store
              no-proceed))
      (numV 22))

(test (v*s-v (interp (plusC (numC 10) (appC 'quadruple (plusC (numC 1) (numC 2))))
              (list (funC 'quadruple 'x (appC 'double (appC 'double (varC 'x))))
                    (funC 'double 'x (seqC (setC 'x (plusC (varC 'x) (varC 'x))) (varC 'x))))
              mt-store
              no-proceed))
      (numV 22))

(test (v*s-v (interp (appC 'change (numC 2))
              (list (aroundC 'change 'y (proceedC (multC (varC 'y) (numC 2))))
                    (aroundC 'change 'y (proceedC (plusC (varC 'y) (numC 3))))
                    (funC 'change 'x (plusC (idC 'x) (numC 5))))
              mt-store
              no-proceed))
      (numV 12))

(test (v*s-v (interp (appC 'change (numC 2))
              (list (aroundC 'change 'y (proceedC (plusC (varC 'y) (numC 3))))
                    (aroundC 'change 'y (proceedC (multC (varC 'y) (numC 2))))
                    (funC 'change 'x (plusC (varC 'x) (numC 5))))
              mt-store
              no-proceed))
      (numV 15))

(test/exn (v*s-v (interp (appC 'foo (numC 2))
              (list (funC 'foo 'x (proceedC (varC 'x))))
              mt-store
              no-proceed))
      "proceed called outside of advice")

(test (v*s-v (interp (writeC "The answer" (numC 42))
              mt-env
              mt-store
              no-proceed))
      (numV 42))

(test (v*s-v (interp (writeC "fact(3)" (appC 'fact (numC 3)))
              (list (aroundC 'fact 'y (letVarC 'result (proceedC (varC 'y))
                                       (seqC (writeC "y" (varC 'y))
                                        (seqC (writeC "result" (varC 'result))
                                              (varC 'result)))))
                    (funC 'fact 'x (ifZeroOrLessC (varC 'x) (numC 1) (multC (varC 'x) (appC 'fact (plusC (varC 'x) (numC -1)))))))
              mt-store
              no-proceed))
      (numV 6))

(test (struct->list/recursive (numC 4)) '(numC 4))
(test (struct->list/recursive (plusC (numC 3) (numC 4))) '(plusC (numC 3) (numC 4)))
(test (struct->list/recursive (appC 'foo (numC 4))) '(appC 'foo (numC 4)))

(define (test-roundtrip e) (test (list->struct/recursive miraj-ns (struct->list/recursive e)) e))

(test-roundtrip (plusC (numC 3) (numC 4)))
(test-roundtrip (appC 'foo (numC 4)))
(test-roundtrip (funC 'foo 'bar (multC (numC 4) (varC 'bar))))