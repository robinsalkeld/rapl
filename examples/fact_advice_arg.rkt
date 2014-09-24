(lambda (f a)
  ((file "examples/around.rkt") 
   ((file "examples/call.rkt") "fact")
   (lambda (proceed y) 
     (proceed (+ y -1)))
   f
   a))