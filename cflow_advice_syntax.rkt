#lang racket
(around (bar) 
  (lambda (proceed) (lambda (x) 
    (around (foo) 
            (<advice functor>)
            (proceed x))))
  (...))
	    
