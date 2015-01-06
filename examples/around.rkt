(lambda (pc advice thunk)
  (lambda (v)
    (aroundapp 
     (lambda (tag g)
       (if (pc tag)
           (advice g)
           g))
     (thunk v))))
