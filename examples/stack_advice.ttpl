(let ([jps (box [])])
     (onapp
          (lambda (f x) 
               (seq (set-box! jps (cons f (unbox jps)))
                    (let ([result (f x)])
                         (seq (set-box! jps (tail (unbox jps)))
                              result))))
          (...)))
