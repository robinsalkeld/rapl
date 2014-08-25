#lang racket
(aroundapp bar 
  (lambda (proceed) (lambda (x) 
    (aroundapp foo
            (<advice functor>)
            (proceed x))))
  (...))
	    
