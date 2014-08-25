; \f -> (\x -> f (\y -> (x x) y) (\x -> f (\y -> (x x) y)
(lambda (f) ((lambda (x) (f (lambda (y) ((x x) y))))
             (lambda (x) (f (lambda (y) ((x x) y))))))
