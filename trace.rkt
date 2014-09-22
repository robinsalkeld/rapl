(lambda (f) (lambda (a)
              (aroundapp
                 (lambda (proceed)
                   (seq (write "call" proceed)
                        proceed))
                 (f a))))
