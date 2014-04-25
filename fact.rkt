(miraj 
 
  empty
 
  (list (fdC 'fact 'x 
             (ifZeroOrLessC (varC 'x) 
                            (numC 1) 
                            (multC (varC 'x) 
                                   (appC 'fact (plusC (varC 'x) (numC -1))))))
        (fdC 'fact_loop 'x 
             (letC 'in (readC) 
                   (ifZeroOrLessC (varC 'in) 
                                  (varC 'in) 
                                  (seqC (writeC (appC 'fact (varC 'in)))
                                        (appC 'fact_loop (varC 'in)))))))
  
  empty
  
  (appC 'fact_loop (numC 42))
)