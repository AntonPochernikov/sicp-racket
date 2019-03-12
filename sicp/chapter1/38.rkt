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

(define (d i)
  (if (= 0 (remainder (+ i 1) 3))
      (* 2 (/ (+ i 1) 3))
      1.0))

(check-equal? (+ 2 (cont-frac-iter (lambda (i) 1.0) d 10))
              (+ 2 (cont-frac-recursive (lambda (i) 1.0) d 10)))

(+ 2 (cont-frac-recursive (lambda (i) 1.0) d 10))

