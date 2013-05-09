(miraj 
       (list (fdC 'fact 'x (ifZeroC (varC 'x) (numC 1) (multC (varC 'x) (appC 'fact (plusC (varC 'x) (numC -1)))))))
       
       (list (aroundC 'fact 'y (letC 'result (proceedC (varC 'y))
                                    (seqC (writeC (varC 'y))
                                     (seqC (writeC (varC 'result))
                                      (varC 'result))))))
       
       (writeC (appC 'fact (numC 3)))
)