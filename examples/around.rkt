(lambda (pc advice f a)
  (onapp 
   (lambda (g)
     (tagtest g
              (lambda (gtag gtagged)
                (if (pc gtag)
                    (tag gtag (advice gtagged))
                    g))
              g))
   (f a)))
