(lambda (f a)
  ((file "around.ttpl") 
   ((file "call.ttpl") 'fact)
   (lambda (proceed y) 
     (proceed (+ y -1)))
   f
   a))
