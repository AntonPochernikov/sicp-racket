#lang racket

(define (distinct-only? item items-list)
  (cond [(null? items-list) true]
        [(eq? item (car items-list)) false]
        [else (distinct-only? item (cdr items-list))]))

 (define (multiple-dwelling)
   (let ([fletcher (amb 1 2 3 4 5)])
     (require (not (= fletcher 5)))
     (require (not (= fletcher 1)))
     (let ([smith (amb 1 2 3 4 5)])
       (require (not (= 1 (abs (- smith fletcher)))))
       (require (distinct-only? smith (list fletcher)))
       (let ([cooper (amb 1 2 3 4 5)])
         (require (distinct-only? cooper (list fletcher smith)))
         (require (not (= 1 (abs (- fletcher cooper)))))
         (require (not (= cooper 1)))
         (let ([miller (amb 1 2 3 4 5)])
           (require (> miller cooper))
           (require (distinct-only? miller (list fletcher smith)))
           (let ([baker (amb 1 2 3 4 5)])
             (require (distinct-only? baker
                                      (list fletcher smith cooper miller)))
             (require (not (= baker 5)))
             (list (list 'baker baker)
                   (list 'cooper cooper)
                   (list 'fletcher fletcher)
                   (list 'miller miller)
                   (list 'smith smith))))))))
    
    

    

