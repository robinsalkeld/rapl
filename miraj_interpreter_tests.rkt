#lang plai

(require "miraj.rkt")
(require "miraj_parser.rkt")
(require "miraj_interpreter.rkt")
(require "miraj_serialization.rkt")
(require "miraj_recording.rkt")

(test (parse '(let ([const5 (lambda (_) 5)])
                 (+ 10 (const5 10))))
      (letC 'const5 (lamC '_ (numC 5)) 
            (plusC (numC 10) (appC (idC 'const5) (numC 10)))))

(test/exn (parse '(+ 1 2 3))
      "parse: Wrong number of arguments")

(test (interp-exp (parse 
                   '(let ([const5 (lambda (_) 5)])
                      (+ 10 (const5 10)))))
      (numV 15))
 
(test (interp-exp (parse 
                   '(let ([double (lambda (x) (+ x x))])
                      (+ 10 (double (+ 1 2))))))
      (numV 16))
 
(test (interp-exp (parse
                   '(let ([double (lambda (x) (+ x x))])
                      (let ([quadruple (lambda (x) (double (double x)))])
                        (+ 10 (quadruple (+ 1 2)))))))
      (numV 22))

(test (interp-exp (parse
                   '((file "untag.rkt") (tag "foo" 42))))
      (numV 42))

(test (interp-exp (parse
                   '(((((file "around.rkt") ((file "call.rkt") "change"))
                                         (lambda (proceed) (lambda (y) (proceed (* y 2)))))
                                         (lambda (dummy)
                                           (((((file "around.rkt") ((file "call.rkt") "change"))
                                                                (lambda (proceed) (lambda (y) (proceed (+ y 3))))) 
                                                                (lambda (dummy)
                                                                  (let ([change (tag "change" (lambda (x) (+ x 5)))])
                                                                    (change 2))))
                                                                0)))
                                         0)))
      (numV 15))

(test (interp-exp (parse
                   '(((((file "around.rkt") ((file "call.rkt") "change"))
                                         (lambda (proceed) (lambda (y) (proceed (+ y 3)))))
                                         (lambda (dummy)
                                           (((((file "around.rkt") ((file "call.rkt") "change"))
                                                                (lambda (proceed) (lambda (y) (proceed (* y 2))))) 
                                                                (lambda (dummy)
                                                                  (let ([change (tag "change" (lambda (x) (+ x 5)))])
                                                                    (change 2))))
                                                                0)))
                                         0)))
      (numV 12))

(test (interp-exp (writeC "The answer" (numC 42)))
      (numV 42))


(test (interp-exp (parse '((file "fact.rkt") 4)))
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

;;(define fact-trace (read-struct-from-file miraj-ns "fact_trace.txt"))

;;(map (lambda (jp) (begin (display-joinpoint jp (current-output-port)) (newline))) (mirajTrace-joinpoints fact-trace))