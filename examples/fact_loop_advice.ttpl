(lambda (f a)
  ;; TODO-RS: aroundbox doesn't make that much sense - the label isn't applied until after 
  ;; the box is constructed!
  (aroundbox 
   "inputs"
   (lambda (inputs)
     ((file "around.ttpl")  
      ((file "call.ttpl") "fact_loop")
      (lambda (proceed y) 
        (let ([result (proceed y)])
          (seq (write "y" y)
               (seq (write "result" result)
                    result)))))
     inputs)
   (f a)))
