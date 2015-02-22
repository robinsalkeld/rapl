#lang plai

;;
;; Rapl AST
;;

(define-type ExprC
  ;; Identifiers and functions
  [idC (s symbol?)]
  [appC (fun ExprC?) (args (listof ExprC?))]
  [lamC (params (listof symbol?)) (body ExprC?)]
  [recC (fun ExprC?)]
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
  [aroundappsC (fun ExprC?) (in ExprC?)]
  ;; Input/Output
  [fileC (path string?)]
  [writeC (l string?) (v ExprC?)]
  [readC (l string?)]
)