#lang plai

(define-type ExprC
  ;; Numbers, arithmetic, and conditionals
  [numC (n number?)]
  [plusC (l ExprC?) (r ExprC?)]
  [multC (l ExprC?) (r ExprC?)]
  [ifZeroC (c ExprC?) (t ExprC?) (f ExprC?)]
  ;; Identifiers and functions
  [idC (s symbol?)]
  [appC (fun ExprC?) (arg ExprC?)]
  [lamC (arg symbol?) (body ExprC?)]
  [letC (s symbol?) (v ExprC?) (in ExprC?)]
  ;; Boxes and sequencing
  [boxC (b ExprC?)]
  [unboxC (b ExprC?)]
  [setboxC (b ExprC?) (v ExprC?)]
  [seqC (b1 ExprC?) (b2 ExprC?)]
  ;; Advice
  [labelC (name symbol?) (v ExprC?)]
  [aroundAppC (name symbol?) (fun ExprC?) (in ExprC?)]
  [aroundSetC (name symbol?) (fun ExprC?) (in ExprC?)]
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

(define read-source (box (lambda () (string->number (read-line)))))

(define interp-input (box '()))
(define (record-interp-input (x number?))
  (set-box! interp-input (cons x (unbox interp-input))))
(define get-interp-input 
  (lambda () (reverse (unbox interp-input))))

