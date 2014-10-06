#lang plai

(require "miraj.rkt")
(require "miraj_parser.rkt")
(require "miraj_interpreter_retroactive.rkt")
(require "miraj_serialization.rkt")

(print-only-errors)

(test (parse '(let ([const5 (lambda (_) 5)])
                 (+ 10 (const5 10))))
      (letC 'const5 (lamC '_ (numC 5)) 
            (plusC (numC 10) (appC (idC 'const5) (numC 10)))))

(test (parse '(lambda (x y z) (x y z)))
      (lamC 'x (lamC 'y (lamC 'z (appC (appC (idC 'x) (idC 'y)) (idC 'z))))))

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
                   '((file "examples/untag.rkt") (tag "foo" 42))))
      (numV 42))

(test (interp-exp (parse
                   '((file "examples/around.rkt") 
                     ((file "examples/call.rkt") "change")
                     (lambda (proceed y) (proceed (* y 2)))
                     (lambda (dummy)
                       ((file "examples/around.rkt") 
                        ((file "examples/call.rkt") "change")
                        (lambda (proceed y) (proceed (+ y 3))) 
                        (lambda (dummy)
                          (let ([change (tag "change" (lambda (x) (+ x 5)))])
                            (change 2)))
                        0))
                     0)))
      (numV 15))

(test (interp-exp (parse
                   '((file "examples/around.rkt") 
                     ((file "examples/call.rkt") "change")
                     (lambda (proceed y) (proceed (+ y 3)))
                     (lambda (dummy)
                       ((file "examples/around.rkt") 
                        ((file "examples/call.rkt") "change")
                        (lambda (proceed) (lambda (y) (proceed (* y 2))))
                        (lambda (dummy)
                          (let ([change (tag "change" (lambda (x) (+ x 5)))])
                              (change 2)))
                        0))
                     0)))
      (numV 12))

(test (interp-exp (writeC "The answer" (numC 42)))
      (numV 42))


(test (interp-exp (parse '((file "examples/fact.rkt") 4)))
      (numV 24))

(test (interp-exp (appC (appC (fileC "examples/fact_advice.rkt") (fileC "examples/fact.rkt")) (numC 3)))
      (numV 6))

(test (struct->list/recursive (numC 4)) '(numC 4))
(test (struct->list/recursive (plusC (numC 3) (numC 4))) '(plusC (numC 3) (numC 4)))
(test (struct->list/recursive (appC (idC 'foo) (numC 4))) '(appC (idC 'foo) (numC 4)))

(define (test-roundtrip e) (test (list->struct/recursive miraj-ns (struct->list/recursive e)) e))

(test-roundtrip (plusC (numC 3) (numC 4)))
(test-roundtrip (appC (idC 'foo) (numC 4)))
(test-roundtrip (lamC 'bar (multC (numC 4) (idC 'bar))))

(test (interp-query "traces/fact_trace.txt" (list (fileC "examples/fact_advice.rkt")))
      (numV 6))

(test/exn (interp-query "traces/fact_trace.txt" (list (fileC "examples/fact_advice_arg.rkt")))
      "retroactive-side-effect: incorrect argument passed retroactively: expected\n #(struct:numV 3) but got\n #(struct:numV 2)")

(test/exn (interp-query "traces/fact_trace.txt" (list (fileC "examples/fact_advice_double_proceed.rkt")))
      "retroactive-side-effect: retroactive advice proceeded more than once")

;;(map (lambda (jp) (begin (display-joinpoint jp (current-output-port)) (newline))) (mirajTrace-joinpoints fact-trace))