(appC (fileC "y-comb.rkt")
      (lamC 'fact 
            (labelC 'fact 
                    (lamC 'x 
                          (ifZeroC (idC 'x) 
                                   (numC 1) 
                                   (multC (idC 'x) (appC (idC 'fact) (plusC (idC 'x) (numC -1)))))))))