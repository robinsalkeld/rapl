(defC (aroundC 'fact 'y (letC 'result (proceedC (varC 'y))
                                    (seqC (writeC "y" (readC "more input"))
                                     (seqC (writeC "result" (varC 'result))
                                      (varC 'result)))))
  
  (proceedC (numC 0)))