#lang sicp

(define (cont-frac-iter n d k)
  (define (iter i acc)
    (let ((ni (n i))
          (di (d i)))
      (if (= i 0)
          acc
          (iter (- i 1) (/ ni (+ di acc))))))
  (iter k 0))

(define (square x) (* x x))
(define (tan-cf x k)
  (cont-frac-iter (lambda (i) (if (= i 1)
                                  x
                                  (- (square x))))
                  (lambda (i) (- (* i 2.0) 1))
                  k))

(display (tan-cf 1 10))
(newline)
(display (tan 1))
