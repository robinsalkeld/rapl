(letInlineC 'fact_loop 'y (seqC (writeC "inputs" (varC 'inputs))
                                     (proceedC (varC 'y)))
      
      (letAroundC 'fact 'y (letVarC 'result (proceedC (varC 'y))
                                    (seqC (writeC "y" (varC 'y))
                                     (seqC (writeC "result" (varC 'result))
                                      (varC 'result))))
            
            (proceedC (numC 0))))