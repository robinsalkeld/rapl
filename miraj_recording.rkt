#lang plai

(require "miraj_interpreter.rkt")
(require "miraj_serialization.rkt")

(define (interp-with-recording (program MirajProgram?) (recording-path path-string?))
  (let* ([result (interp-program program)]
         [input (get-interp-input)]
         [recording (mirajRecForReplay program input)]
         [_ (write-struct-to-file recording recording-path)])
    result))
           
(define (replay-interp (recording-path path-string?))
  (let* ([recording (read-struct-from-file recording-path)]
         [remaining-input (box (mirajRecForReplay-input recording))]
         [_ (set-box! read-source (lambda ()
                                    (let* ([next (car (unbox remaining-input))]
                                           [_ (set-box! remaining-input (cdr (unbox remaining-input)))])
                                      next)))])
    (interp-program (mirajRecForReplay-program recording))))
         
                                           
  