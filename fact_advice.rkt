(advC (aroundC 'fact 'y (letC 'result (proceedC (varC 'y))
                                    (seqC (writeC (varC 'y))
                                     (seqC (writeC (varC 'result))
                                      (varC 'result)))))
      
       (proceedC (numC 0)))