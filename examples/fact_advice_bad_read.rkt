(lambda (f a)
  ((file "examples/around.rkt") ((file "examples/call.rkt") fact)
                                (lambda (proceed y) 
                                  (let ([result (proceed y)])
                                    (seq (write "y" (read "more input"))
                                         (seq (write "result" result)
                                              result))))
                                f 
                                a))