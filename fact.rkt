(miraj 
       (list (fdC 'fact 'x (ifZeroOrLessC (varC 'x) (numC 1) (multC (varC 'x) (appC 'fact (plusC (varC 'x) (numC -1))))))
             (fdC 'fact-loop 'x 
                  (letC 'in (readC) 
                        (ifZeroOrLessC (varC 'in) 
                                       (varC 'in) 
                                       (seqC (writeC (appC 'fact (varC 'in)))
                                             (appC 'fact-loop (varC 'in))
                                             )
                                       )
                        )
                  )
             )
       
       
       empty
       
       (writeC (appC 'fact-loop (numC 42)))
)