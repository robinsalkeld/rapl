(letC 'inputs (numC 0)
      
      (defC (funC 'fact 'x 
                  (ifZeroOrLessC (varC 'x) 
                                 (numC 1) 
                                 (multC (varC 'x) 
                                        (appC 'fact (plusC (varC 'x) (numC -1))))))
      
            (defC (funC 'fact_loop 'x 
                        (letC 'in (readC "input")
                              (ifZeroOrLessC (varC 'in) 
                                             (varC 'in) 
                                             (seqC (writeC "input!" (appC 'fact (varC 'in)))
                                                   (seqC (setC 'inputs (plusC (varC 'inputs) (numC 1)))
                                                         (appC 'fact_loop (varC 'in)))))))
             
                  (appC 'fact_loop (numC 0)))))