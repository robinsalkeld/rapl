(lambda (f a)
  ((file "examples/around.rkt") 
   ((file "examples/call.rkt") "fact")
   (lambda (proceed y) 
     (let ([result (proceed y)])
       (if (equal? y 0)
           7
           result)))
   f
   a))