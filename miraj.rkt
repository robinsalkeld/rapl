#lang plai

(define-type ExprC
  ;; Identifiers and functions
  [idC (s symbol?)]
  [appC (fun ExprC?) (arg ExprC?)]
  [lamC (arg symbol?) (body ExprC?)]
  [letC (s symbol?) (v ExprC?) (in ExprC?)]
  ;; Numbers and arithmetic
  [numC (n number?)]
  [plusC (l ExprC?) (r ExprC?)]
  [multC (l ExprC?) (r ExprC?)]
  ;; Booleans and conditionals
  [boolC (b boolean?)]
  [equalC (l ExprC?) (r ExprC?)]
  [ifC (c ExprC?) (t ExprC?) (f ExprC?)]
  ;; Boxes and sequencing
  [boxC (b ExprC?)]
  [unboxC (b ExprC?)]
  [setboxC (b ExprC?) (v ExprC?)]
  [seqC (b1 ExprC?) (b2 ExprC?)]
  [voidC]
  ;; Advice
  [symbolC (s symbol?)]
  [tagC (tag ExprC?) (v ExprC?)]
  [aroundAppC (fun ExprC?) (in ExprC?)]
  ;; Input/Output
  [fileC (path string?)]
  [writeC (l string?) (v ExprC?)]
  [readC (l string?)]
)

(define (list-box-push! b x)
  (set-box! b (cons x (unbox b))))
(define (list-box-pop! b)
  (let* ([next (first (unbox b))]
         [_ (set-box! b (rest (unbox b)))])
         next))

(define read-source (box (lambda (prompt) 
                           (display prompt)
                           (display "> ")
                           (string->number (read-line)))))
(define write-sink (box (lambda (s) (begin (display s) (newline)))))

(define interp-input (box '()))

;; TODO-RS: Move to miraj_recording?
(define (record-interp-input (x number?))
  (list-box-push! interp-input x))
(define get-interp-input 
  (lambda () (reverse (unbox interp-input))))

