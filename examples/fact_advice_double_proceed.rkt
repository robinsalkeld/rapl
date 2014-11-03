(lambda (f a)
  ((file "examples/around.rkt") 
   ((file "examples/call.rkt") 'fact)
   (lambda (proceed y) 
     (seq (proceed y) (proceed y)))
   f
   a))