(letC 'inputs (boxC (numC 0))
      (letC 'fact (fileC "fact.rkt")
            (appC (fileC "y-comb.rkt")
                  (lamC 'fact_loop
                        (labelC 'fact_loop
                                (lamC 'dummy 
                                      (letC 'in (readC "x")
                                            (ifZeroC (idC 'in) 
                                                     (idC 'in) 
                                                     (seqC (writeC "fact(x)" (appC (idC 'fact) (idC 'in)))
                                                           (seqC (setboxC (idC 'inputs) (plusC (unboxC (idC 'inputs)) (numC 1)))
                                                                 (appC (idC 'fact_loop) (idC 'in))))))))))))