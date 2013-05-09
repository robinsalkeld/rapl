#lang plai

(require "miraj_interpreter.rkt")
(require "miraj_serialization.rkt")

;;
;; Miraj interpreter CLI
;;

(define (interp-program [mp MirajProgram?])
  (interp (miraj-exp mp) mt-env (miraj-fds mp) (miraj-ads mp) mt-store no-proceed)
)

(define program-path (command-line #:args (filename) filename))
(define program (read-struct-from-file program-path))
(begin (write (v*s-v (interp-program program))) (newline))
