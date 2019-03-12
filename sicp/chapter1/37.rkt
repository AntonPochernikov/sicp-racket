#lang sicp

(#%require rackunit)

(define (cont-frac-iter n d k)
  (define (iter i acc)
    (let ((ni (n i))
          (di (d i)))
      (if (= i 0)
          acc
          (iter (- i 1) (/ ni (+ di acc))))))
  (iter k 0))

(define (cont-frac-recursive n d k)
  (define (frac i)
    (let ((ni (n i))
          (di (d i)))
      (if (= i k)
          (/ ni di)
          (/ ni (+ di (frac (inc i)))))))
  (frac 1))


(check-equal? (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 11)
             (cont-frac-recursive (lambda (i) 1.0) (lambda (i) 1.0) 11))


