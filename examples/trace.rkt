(lambda (f a)
  (onapp
   (lambda (proceed)
     (seq (write "call" proceed)
          proceed))
   (f a)))
