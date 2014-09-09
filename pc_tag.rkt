(lambda (tag) (lambda (advice) (lambda (f) (lambda (a)
  (aroundapp (lambda (g)
               (tagtest g
                        (lambda (gtag) (lambda (gtagged)
                                         (if (equal? tag gtag)
                                             (advice gtagged)
                                             gtagged)))
                        g))
             (f a))))))