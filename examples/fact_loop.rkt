(let ([inputs (tag 'inputs (box 0))])
  (let ([fact (file "examples/fact.alpha")])
    ((file "examples/y-comb.rkt")
     (lambda (fact_loop)
       (tag 'fact_loop
            (lambda (dummy)
              (let ([in (read "x")])
                (if (equal? in 0) 
                    in 
                    (seq (write "fact(x)" (fact in))
                         (seq (set-box! inputs (+ (unbox inputs) 1))
                              (fact_loop dummy)))))))))))