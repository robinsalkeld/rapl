(letC (aroundC 'fact 'y (letC (bindC 'result (proceedC (varC 'y)))
                                    (seqC (writeC (varC 'y))
                                     (seqC (writeC (varC 'result))
                                      (varC 'result)))))
      
       (proceedC (numC 42)))