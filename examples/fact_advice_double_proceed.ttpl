(lambda (f a)
  ((file "around.ttpl") 
   ((file "call.ttpl") 'fact)
   (lambda (proceed y) 
     (seq (proceed y) (proceed y)))
   f
   a))
