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
      "#<procedure:plusC27> \"Wrong number of arguments\"")

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
                   '((file "examples/around.rkt") 
                     ((file "examples/call.rkt") 'change)
                     (lambda (proceed y) (proceed (* y 2)))
                     (lambda (dummy)
                       ((file "examples/around.rkt") 
                        ((file "examples/call.rkt") 'change)
                        (lambda (proceed y) (proceed (+ y 3))) 
                        (lambda (dummy)
                          (let ([change (tag 'change (lambda (x) (+ x 5)))])
                            (change 2)))
                        0))
                     0)))
      (numV 15))

(test (interp-exp (parse
                   '((file "examples/around.rkt") 
                     ((file "examples/call.rkt") 'change)
                     (lambda (proceed y) (proceed (+ y 3)))
                     (lambda (dummy)
                       ((file "examples/around.rkt") 
                        ((file "examples/call.rkt") 'change)
                        (lambda (proceed) (lambda (y) (proceed (* y 2))))
                        (lambda (dummy)
                          (let ([change (tag 'change (lambda (x) (+ x 5)))])
                              (change 2)))
                        0))
                     0)))
      (numV 12))

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
      (v*o (numV 42) '("The answer: 42")))

(test (interp-exp (parse '((file "examples/fact.alpha") 4)))
      (numV 24))

(test (interp-exp-with-output (appC (appC (fileC "examples/fact_advice.alpha") (fileC "examples/fact.alpha")) (numC 3)))
      (v*o (numV 6) '("y: 0"
                      "result: 1"
                      "y: 1"
                      "result: 1"
                      "y: 2"
                      "result: 2"
                      "y: 3"
                      "result: 6")))

(test (interp-exp-with-output (appC (appC (fileC "examples/fact_boxes_advice.rkt") (fileC "examples/fact_boxes.alpha")) (numC 3)))
      (v*o (numV 6) '("y before: 3"
                      "y before: 2"
                      "y before: 1"
                      "y before: 0"
                      "y after: 42"
                      "result: 1"
                      "y after: 42"
                      "result: 1"
                      "y after: 42"
                      "result: 2"
                      "y after: 42"
                      "result: 6")))

(test (struct->list/recursive (numC 4)) '(numC 4))
(test (struct->list/recursive (plusC (numC 3) (numC 4))) '(plusC (numC 3) (numC 4)))
(test (struct->list/recursive (appC (idC 'foo) (numC 4))) '(appC (idC 'foo) (numC 4)))

(define (test-roundtrip e) (test (list->struct/recursive miraj-ns (struct->list/recursive e)) e))

(test-roundtrip (plusC (numC 3) (numC 4)))
(test-roundtrip (appC (idC 'foo) (numC 4)))
(test-roundtrip (lamC 'bar (multC (numC 4) (idC 'bar))))


(define (interp-query-with-output [trace-file string?] [exps list?]) ValueWithOutput?
  (with-output (lambda () (interp-query trace-file exps))))

(test (interp-query-with-output "traces/fact_trace.txt" (list (fileC "examples/fact_advice.alpha")))
      (v*o (numV 6) '("y: 0"
                      "result: 1"
                      "y: 1"
                      "result: 1"
                      "y: 2"
                      "result: 2"
                      "y: 3"
                      "result: 6")))

(test/exn (interp-query "traces/fact_trace.txt" (list (fileC "examples/fact_advice_arg.rkt")))
      "retroactive-side-effect: incorrect argument passed retroactively: expected\n #(struct:numV 3) but got\n #(struct:numV 2)")

(test/exn (interp-query "traces/fact_trace.txt" (list (fileC "examples/fact_advice_base_case.rkt")))
      "retroactive-side-effect: incorrect retroactive result: expected\n #(struct:numV 1) but got\n #(struct:numV 7)")

(test/exn (interp-query "traces/fact_trace.txt" (list (fileC "examples/fact_advice_double_proceed.rkt")))
      "retroactive-side-effect: retroactive advice proceeded out of order")

(test/exn (interp-query "traces/fact_trace.txt" (list (fileC "examples/fact_advice_bad_read.rkt")))
      "retroactive-side-effect: attempt to retroactively read input")

(test (interp-query-with-output "traces/fact_boxes_trace.txt" (list (fileC "examples/fact_boxes_advice.rkt")))
      (v*o (numV 6) '("y before: 3"
                      "y before: 2"
                      "y before: 1"
                      "y before: 0"
                      "y after: 42"
                      "result: 1"
                      "y after: 42"
                      "result: 1"
                      "y after: 42"
                      "result: 2"
                      "y after: 42"
                      "result: 6")))

(test/exn (interp-query-with-output "traces/fact_boxes_trace.txt" (list (fileC "examples/fact_boxes_advice_bad_set_box.rkt")))
      "retroactive-side-effect: attempt to retroactively set box")

(define t-sto (store (list (cell 0 (numV 5)) (cell 1 (boxV 2)) (cell 2 (numV 6)) (cell 3 (boxV 4)) (cell 4 (numV 42)) (cell 5 (boxV 6)) (cell 6 (boxV 5))) mt-trace))
(define sto-cells (list (cell 0 (numV 7)) (mapping 1 0) (mapping 2 1) (mapping 3 2)))
(define sto (store sto-cells (list (state (app-result (numV 0)) mt-adv t-sto))))

(test (fetch sto 0) (numV 7))
(test (fetch sto 1) (numV 5))
(test (fetch sto 2) (boxV 3))

(type-case Result (map-box 3 sto)
  (v*s*t (v s t)
         (let ([b (fetch s (boxV-l v))])
           (test (fetch s (boxV-l b)) (numV 42)))))

;; Watch out for infinite recursion on recursive data
(type-case Result (map-box 5 sto)
  (v*s*t (v s t)
         (test v (boxV 4))))
 
;; Check the unsafe but much simpler versions used for the purpose of presentation

(set-box! retroactive-error-checking #f)

(test (interp-exp-with-output (appC (appC (fileC "examples/fact_advice.alpha") (fileC "examples/fact.alpha")) (numC 3)))
      (v*o (numV 6) '("y: 0"
                      "result: 1"
                      "y: 1"
                      "result: 1"
                      "y: 2"
                      "result: 2"
                      "y: 3"
                      "result: 6")))

;;(map (lambda (jp) (begin (display-joinpoint jp (current-output-port)) (newline))) (mirajTrace-joinpoints fact-trace))

(display "Done!")