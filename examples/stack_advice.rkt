
(define (aroundlabel (label advice f a))
  (aroundapp (lambda (g)
               (tagtest g
                        (lambda (gtag) (lambda (gtagged)
                                         (if (equal? label gtag)
                                             (advice gtagged)
                                             gtagged)))
                        g))
             (f a)))
               

(let ([jps (box [])])
     (aroundapp
          (lambda (f) (lambda (x) 
               (seq (set-box! jps (cons f (unbox jps)))
                    (let ([result (f x)])
                         (seq (set-box! jps (rest (unbox jps)))
                              result)))))
          (...)))

(letrec ([stackadvice
          (lambda (pc advice)
            (lambda (f) (lambda (x)
              (tagtest f
                       (lambda (tag) (lambda (f/untagged)
                                       (let ([jps (cons tag above)])
                                         (if (pc jps)
                                             (around (stackadvice jps) ((advice f/untagged) x))
                                             (around (stackadvice jps) (f/untagged x))))))
                       (aroundapp (stackadvice jps) (f/untagged x))))))])
  (aroundapp (stackadvice pc advice) ((label () foo) bar)))

(let ([app/prim (lambda (f) (tagtest f (lambda (_) (lambda (f/untagged) f/untagged) f)))])
  ...)