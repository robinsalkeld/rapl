(lambda (f a)
  ((file "around.ttpl") 
   ((file "call.ttpl") 'fact)
   (lambda (proceed y) 
     (let ([result (proceed y)])
       (if (equal? y 0)
           7
           result)))
   f a))
