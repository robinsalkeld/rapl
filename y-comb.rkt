; \f -> (\x -> f (\y -> (x x) y) (\x -> f (\y -> (x x) y)
(lamC 'f (appC (lamC 'x (appC (idC 'f) (lamC 'y (appC (appC (idC 'x) (idC 'x)) (idC 'y))))) 
               (lamC 'x (appC (idC 'f) (lamC 'y (appC (appC (idC 'x) (idC 'x)) (idC 'y)))))))