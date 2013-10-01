#lang plai

(require "miraj_interpreter.rkt")
(require "miraj_serialization.rkt")
(require "miraj_recording.rkt")

;;
;; Miraj interpreter CLI
;;

(define recording-path (make-parameter #f))
(define replay-path (make-parameter #f))
(define files (make-parameter '()))

(command-line 
 #:once-each
 [("-r" "--recording") r "Record execution" (recording-path r)]
 [("-r" "--replay") r "Replay execution" (replay-path r)]
 #:args fs (files fs))

(define program (lambda () (read-struct-from-file (list-ref (files) 0))))

(let ([result 
       (cond
         [(recording-path) (interp-with-recording (program) (recording-path))]
         [(replay-path) (replay-interp (replay-path))]
         [else (interp-program (program))])])
  (begin (write (v*s-v result)) (newline)))
