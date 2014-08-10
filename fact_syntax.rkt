#lang racket
(let ([Y (...)])
  (Y (lambda (fact)
       (lambda (x)
         (if0 x
              1  
              (* x (fact (+ x -1))))))))