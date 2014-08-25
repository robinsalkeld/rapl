(lambda (f) (lambda (a)
                  (aroundapp ([fact 
                               (lambda (proceed) (lambda (y) 
                                                   (let ([result (proceed y)])
                                                     (seq (write "y" y)
                                                          (seq (write "result" result)
                                                               result)))))])
                             (f a))))