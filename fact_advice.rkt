(lamC 'f (lamC 'a
                  (aroundAppC 'fact 
                              (lamC 'proceed 
                                    (lamC 'y (letC 'result (appC (idC 'proceed) (idC 'y))
                                                   (seqC (writeC "y" (idC 'y))
                                                         (seqC (writeC "result" (idC 'result))
                                                               (idC 'result))))))
                              (appC (idC 'f) (idC 'a)))))