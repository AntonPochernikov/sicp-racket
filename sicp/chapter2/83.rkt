#lang sicp

(define (integer->rational x)
  (make-rational x 1))
(put-coersion 'integer 'rational integer->rational)

(define (rational->real x)
  (make-real (/ (numer x) (denom x))))
(put-coersion 'rational 'real rational->real)

(define (real->complex x)
  (make-complex-from-real-imag x 0))
(put-coersion 'real 'complex real->complex)

(define (raise x)
  (define type-tower '(integer rational real complex))
  (let ((type (type-tag x)))
    (define (try-tower tower)
      (if (null? tower)
          (error "can not raise this type" x type-tower)
          (let ((current (car tower))
                (next (cadr tower))
                (rest (cdr tower)))
            (if (eq? current type)
                (if next
                    ((get-coersion type next) x)
                    (error "can not raise this type" x type-tower))
                (try-tower rest)))))
    (try-tower tower)))
