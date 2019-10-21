#lang racket

(require sicp)

; a
(define (letrec->let exp)
  (let ([clauses (let-clauses exp)]
        [body (let-body exp)])
    (if (empty-clauses? clauses)
        body
        (let ([params (map clause-param clauses)]
              [args (map clause-arg clauses)])
          (make-let (map (lambda (p) (list p '*unassigned*))
                         params)
                    (append
                     (map (lambda (p a) (list 'set! p a))
                          params
                          args)
                     body))))))

; b
; The main difference with let is that lambdas for odd? and even?
; will be scoped in (f 5) frame where this procedures are created.
; But name binding happens in another frame that where we call
; (lambda (even? odd?) ...) with its lambda arguments

; That means we won't be able to call one procedure from another

















