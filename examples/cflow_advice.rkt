(around 
  (lambda (t) (equal? t “bar”))
  (lambda (proceed x) 
    (around 
      (lambda (t) (equal? t “foo”))
      <advice functor>
      proceed 
      x))
  (...))
