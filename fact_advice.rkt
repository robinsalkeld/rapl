(miraj 
       empty
       empty
       
       (list (aroundC 'fact 'y (letC 'result (proceedC (varC 'y))
                                    (seqC (writeC (varC 'y))
                                     (seqC (writeC (varC 'result))
                                      (varC 'result))))))
       
       (numC 42)
)