#lang sicp

(define (make-operation-exp expression machine labels operations)
  (let ([operation
         (lookup-operation (operation-exp-op expression)
                           operations)]
        [aprocs
         (map (lambda (exp)
                (if (label-exp? exp)
                    (error "Invalid operation expression: ASSEMBLE"
                           exp)
                    (make-primitive-exp exp machine labels)))
              (operation-exp-operands expression))])
    (lambda ()
      (apply operation
             (map (lambda (p) (p))
                  aprocs)))))
