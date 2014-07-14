(letVarC 'inputs (numC 0)
      
      (letFunC 'fact 'x (ifZeroOrLessC (varC 'x) 
                                       (numC 1) 
                                       (multC (varC 'x) 
                                              (appC 'fact (plusC (varC 'x) (numC -1)))))
      
               (letFunC 'fact_loop 'dummy (letVarC 'in (readC "x")
                                               (ifZeroOrLessC (varC 'in) 
                                                              (varC 'in) 
                                                              (seqC (writeC "fact(x)" (appC 'fact (varC 'in)))
                                                                    (seqC (setC 'inputs (plusC (varC 'inputs) (numC 1)))
                                                                          (appC 'fact_loop (varC 'in))))))
             
                  (appC 'fact_loop (numC 0)))))