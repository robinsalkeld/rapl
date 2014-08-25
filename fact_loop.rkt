(let ([inputs (box 0)])
  (let ([fact (file "fact.rkt")])
    ((file "y-comb.rkt")
     (lambda (fact_loop)
       (label fact_loop
              (lambda (dummy)
                (let ([in (read "x")])
                  (if0 in 
                       in 
                       (seq (write "fact(x)" (fact in))
                            (seq (set-box! inputs (+ (unbox inputs) 1))
                                 (fact_loop dummy)))))))))))