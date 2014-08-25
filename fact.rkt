((file "y-comb.rkt")
      (lambda (fact) 
            (label fact 
                    (lambda (x) 
                          (if0 x 
                               1 
                               (* x (fact (+ x -1))))))))