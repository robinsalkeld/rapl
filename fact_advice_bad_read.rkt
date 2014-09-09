(lambda (f) (lambda (a)
              (aroundapp fact 
                         (lambda (proceed) (lambda (y) 
                                             (let ([result (proceed y)])
                                               (seq (write "y" (read "more input"))
                                                    (seq (write "result" result)
                                                         result)))))
                         (f a))))