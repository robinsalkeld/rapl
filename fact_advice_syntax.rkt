#lang racket
(around (fact) 
        (lambda (proceed) 
          (lambda (x)
            (let ([result (proceed x)])
              (seq (write “y” y)
                   (seq (write “result” result)
                        result)))))
        (...))