(lambda (pc advice f a)
  (aroundapp 
   (lambda (tag g)
     (if (pc tag)
         (advice g)
         g))
   (f a)))
