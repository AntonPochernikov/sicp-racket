#lang sicp

(#%require sicp-pict)

(define (split orientation grow)
  (lambda (painter k)
    (define (iter n)
      (if (= n 0)
          painter
          (let ((smaller (iter (- n 1))))
            (orientation painter (grow smaller smaller)))))
    (iter k)))

(define right-split (split beside below))
(define up-split (split below beside))

(paint (right-split einstein 4))
(paint (up-split einstein 4))
