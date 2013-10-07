(miraj 
       empty
       
       (list (aroundC 'fact 'y (letC 'result (proceedC (varC 'y))
                                    (seqC (writeC (readC))
                                     (seqC (writeC (varC 'result))
                                      (varC 'result))))))
       
       (numC 42)
)