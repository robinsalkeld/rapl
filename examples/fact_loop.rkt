(let ([fact (file "examples/fact.alpha")])
  ((file "examples/y-comb.rkt")
   (lambda (fact_loop)
     (tag 'fact_loop
          (lambda (v)
            (let ([in (read "x")])
              (if (equal? in 0) 
                  in 
                  (seq (write "fact(x)" (fact in))
                       (fact_loop v)))))))))