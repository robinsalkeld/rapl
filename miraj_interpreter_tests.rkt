#lang plai

(require "miraj_interpreter.rkt")
(require "miraj_serialization.rkt")

(read-line)

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

(test (v*s-v (interp (writeC (numC 42))
              mt-env
              empty
              no-advice
              mt-store
              no-proceed))
      (numV 42))

(test (v*s-v (interp (writeC (appC 'fact (numC 3)))
              mt-env
              (list (fdC 'fact 'x (ifZeroC (varC 'x) (numC 1) (multC (varC 'x) (appC 'fact (plusC (varC 'x) (numC -1)))))))
              (list (aroundC 'fact 'y (letC 'result (proceedC (varC 'y))
                                       (seqC (writeC (varC 'y))
                                        (seqC (writeC (varC 'result))
                                              (varC 'result))))))
              mt-store
              no-proceed))
      (numV 6))

(test (struct->list/recursive (numC 4)) '(numC 4))
(test (struct->list/recursive (plusC (numC 3) (numC 4))) '(plusC (numC 3) (numC 4)))
(test (struct->list/recursive (appC 'foo (numC 4))) '(appC 'foo (numC 4)))

(define (test-roundtrip e) (test (list->struct/recursive (struct->list/recursive e)) e))

(test-roundtrip (plusC (numC 3) (numC 4)))
(test-roundtrip (appC 'foo (numC 4)))
(test-roundtrip (fdC 'foo 'bar (multC (numC 4) (varC 'bar))))