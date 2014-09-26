(let ([fact_recursive 
       ((file "examples/y-comb.rkt")
        (lambda (fact) 
          (tag "fact_boxes"
               (lambda (bx)
                 (let ([x (unbox bx)])
                   (if (equal? x 0) 
                       1 
                       (seq (set-box! bx (+ x -1))
                            (* x (fact bx)))))))))])
  (lambda (x) (fact_recursive (box x))))