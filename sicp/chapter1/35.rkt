#lang sicp

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


(display "phi ** 2 = phi + 1")
(newline)
(display "phi ** 2 - phi = 1")
(newline)
(display "phi * (phi - 1) = 1")
(newline)
(display "phi - 1 = 1 / phi")
(newline)
(display "phi = 1 + 1 / phi")
(newline)

(define phi (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
(newline)
(display "phi = ")
(display phi)