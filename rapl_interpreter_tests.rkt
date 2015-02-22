#lang plai

(require "rapl.rkt")
(require "rapl_parser.rkt")
(require "rapl_interpreter.rkt")
(require "rapl_serialization.rkt")

(print-only-errors)
(current-directory (build-path (current-directory) "examples"))

(test (parse '(let ([const5 (lambda (_) 5)])
                 (+ 10 (const5 10))))
      (letC 'const5 (lamC '(_) (numC 5)) 
            (plusC (numC 10) (appC (idC 'const5) (list (numC 10))))))

(test (parse '(lambda (x y z) (x y z)))
      (lamC '(x y z) (appC (idC 'x) (list (idC 'y) (idC 'z)))))

(test/exn (parse '(+ 1 2 3))
      "#<procedure:plusC29> \"Wrong number of arguments\"")

(test (parse '(quote a)) (symbolC 'a))

(test (parse '(void)) (voidC))

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
                   '(let ([sum (lambda (x y z) (+ x (+ y z)))])
                      (sum 1 2 3))))
      (numV 6))

(test (interp-exp (parse '((file "fact.ttpl") 4)))
      (numV 24))

(test (interp-exp (parse
                   '(((file "around.ttpl") 
                      ((file "call.ttpl") 'change)
                      (lambda (proceed) (lambda (y) (proceed (* y 2))))
                      (lambda ()
                        (let ([change (tag 'change (lambda (x) (+ x 5)))])
                          (change 2)))))))
      (numV 9))

(test (interp-exp (parse
                   '(((file "around.ttpl") 
                      ((file "call.ttpl") 'change)
                      (lambda (proceed) (lambda (y) (proceed (* y 2))))
                      ((file "around.ttpl") 
                       ((file "call.ttpl") 'change)
                       (lambda (proceed) (lambda (y) (proceed (+ y 3))))
                       (lambda ()
                         (let ([change (tag 'change (lambda (x) (+ x 5)))])
                           (change 2))))))))
      (numV 12))

(test (interp-exp (parse
                   '(((file "around.ttpl") 
                      ((file "call.ttpl") 'change)
                      (lambda (proceed) (lambda (y) (proceed (+ y 3))))
                      ((file "around.ttpl") 
                       ((file "call.ttpl") 'change)
                       (lambda (proceed) (lambda (y) (proceed (* y 2))))
                       (lambda ()
                         (let ([change (tag 'change (lambda (x) (+ x 5)))])
                           (change 2))))))))
      (numV 15))

(define-type ValueWithOutput
  [v*o (v Value?) (o (listof string?))])

(define (with-output [p procedure?]) ValueWithOutput?
  (let* ([output (box '())]
         [old-sink (unbox write-sink)]
         [_ (set-box! write-sink (lambda (s) (list-box-push! output s)))]
         [result (p)]
         [__ (set-box! write-sink old-sink)])
    (v*o result (reverse (unbox output)))))

(define (interp-exp-with-output [exp ExprC?]) ValueWithOutput?
  (with-output (lambda () (interp-exp exp))))

(test (interp-exp-with-output (writeC "The answer" (numC 42)))
      (v*o (voidV) '("The answer: 42")))

(test (interp-exp-with-output (parse '(((file "fact_advice.ttpl") (lambda () ((file "fact.ttpl") 3))))))
      (v*o (numV 6) '("y: 3"
                      "y: 2"
                      "y: 1"
                      "y: 0"
                      "result: 1"
                      "result: 1"
                      "result: 2"
                      "result: 6")))

(test (interp-exp-with-output (parse '(((file "fact_boxes_advice.ttpl") (lambda () ((file "fact_boxes.ttpl") 3))))))
      (v*o (numV 6) '("y before: 3"
                      "y before: 2"
                      "y before: 1"
                      "y before: 0"
                      "y after: 0"
                      "result: 1"
                      "y after: 0"
                      "result: 1"
                      "y after: 0"
                      "result: 2"
                      "y after: 0"
                      "result: 6")))

(test (interp-exp-with-output (fileC "double_trace_of_double_thunk.ttpl"))
      (v*o (numV 4) '("call: tag2"
                      "call: tag2"
                      "call: tag1"
                      "call: tag1")))

(test (struct->list/recursive (numC 4)) '(numC 4))
(test (struct->list/recursive (plusC (numC 3) (numC 4))) '(plusC (numC 3) (numC 4)))
(test (struct->list/recursive (appC (idC 'foo) (list (numC 4)))) '(appC (idC 'foo) (list (numC 4))))
(test (struct->list/recursive (lamC (list 'foo) (idC 'foo))) '(lamC (list 'foo) (idC 'foo)))

(define (test-roundtrip e) (test (list->struct/recursive miraj-ns (struct->list/recursive e)) e))

(test-roundtrip (plusC (numC 3) (numC 4)))
(test-roundtrip (appC (idC 'foo) (list (numC 4))))
(test-roundtrip (lamC (list 'bar) (multC (numC 4) (idC 'bar))))


(define (interp-query-with-output [trace-file string?] [exps list?]) ValueWithOutput?
  (with-output (lambda () (interp-query trace-file exps))))

; Create fact_trace.txt for the next few tests
(define fact-trace-path "traces/fact_trace.txt")
(test (interp-with-tracing (list (parse '((file "fact.ttpl") 3))) fact-trace-path)
      (numV 6))

(test (interp-query-with-output fact-trace-path (list (fileC "fact_advice.ttpl")))
      (v*o (numV 6) '("y: 3"
                      "y: 2"
                      "y: 1"
                      "y: 0"
                      "result: 1"
                      "result: 1"
                      "result: 2"
                      "result: 6")))

(test/exn (interp-query "traces/fact_trace.txt" (list (fileC "fact_advice_arg.ttpl")))
      "retroactive-side-effect: incorrect argument passed retroactively: expected\n (#(struct:numV 3)) but got\n (#(struct:numV 2))")

(test/exn (interp-query "traces/fact_trace.txt" (list (fileC "fact_advice_base_case.ttpl")))
      "retroactive-side-effect: incorrect retroactive result: expected\n #(struct:numV 1) but got\n #(struct:numV 7)")

(test/exn (interp-query "traces/fact_trace.txt" (list (fileC "fact_advice_no_proceed.ttpl")))
      "retroactive-side-effect: retroactive advice did not proceed")

(test/exn (interp-query "traces/fact_trace.txt" (list (fileC "fact_advice_double_proceed.ttpl")))
      "retroactive-side-effect: retroactive advice proceeded out of order")

(test/exn (interp-query "traces/fact_trace.txt" (list (fileC "fact_advice_bad_read.ttpl")))
      "retroactive-side-effect: attempt to retroactively read input")

; Create fact_boxes_trace.txt for the next few tests
(define fact-boxes-trace-path "traces/fact_boxes_trace.txt")
(test (interp-with-tracing (list (parse '((file "fact_boxes.ttpl") 3))) fact-boxes-trace-path)
      (numV 6))


(test (interp-query-with-output fact-boxes-trace-path (list (fileC "fact_boxes_advice.ttpl")))
      (v*o (numV 6) '("y before: 3"
                      "y before: 2"
                      "y before: 1"
                      "y before: 0"
                      "y after: 0"
                      "result: 1"
                      "y after: 0"
                      "result: 1"
                      "y after: 0"
                      "result: 2"
                      "y after: 0"
                      "result: 6")))

(test/exn (interp-query-with-output "traces/fact_boxes_trace.txt" (list (fileC "fact_boxes_advice_bad_set_box.ttpl")))
      "retroactive-side-effect: attempt to retroactively set box")

(test (interp-exp-with-output (fileC "test_advice_in_trace_inline.ttpl"))
      (v*o (numV 6) '("call: fact"
                      "call: fact"
                      "call: fact"
                      "call: fact"
                      "call: fact_helper"
                      "call: fact_helper"
                      "call: fact_helper"
                      "call: fact_helper"
                      "check: #t")))

; Create fact_traced_trace.txt for the next few tests
(define fact-traced-trace-path "traces/fact_traced_trace.txt")
(test (interp-with-tracing (list (parse '(((file "trace.ttpl") (lambda () ((file "fact.ttpl") 3)))))) fact-traced-trace-path)
      (numV 6))

(test/exn (interp-query-with-output fact-traced-trace-path (list (fileC "inject_fact_boxes.ttpl")))
      "retroactive-side-effect: traces with advice are not supported")

(define empty-trace-path "traces/empty_trace.txt")
(test (interp-with-tracing (list (parse '42)) empty-trace-path)
      (numV 42))

(test (interp-query-with-output empty-trace-path (list (fileC "fact_advice.ttpl")))
      (v*o (numV 42) '()))

;; Check the unsafe but much simpler versions used for the purpose of presentation

(set-box! retroactive-error-checking #f)

(test (interp-query-with-output fact-boxes-trace-path (list (fileC "fact_boxes_advice.ttpl")))
      (v*o (numV 6) '("y before: 3"
                      "y before: 2"
                      "y before: 1"
                      "y before: 0"
                      "y after: 0"
                      "result: 1"
                      "y after: 0"
                      "result: 1"
                      "y after: 0"
                      "result: 2"
                      "y after: 0"
                      "result: 6")))

;;(map (lambda (jp) (begin (display-joinpoint jp (current-output-port)) (newline))) (mirajTrace-joinpoints fact-trace))

(display "Done!")