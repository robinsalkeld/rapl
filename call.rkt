(lambda (label)
  (lambda (f)
    (tagtest f
             (lambda (tag) (lambda (_)
                           (equal? tag label)))
             false)))
                               