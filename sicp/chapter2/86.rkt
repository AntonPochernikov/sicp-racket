#lang sicp

; we need to change all our +, -, * and / operations in complex packages for our generic interface:
; add, sub, mul and div

(define (install-complex-package)
  ; other package procedures

  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))

  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))

  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))

  ; and add interface to this package for the rest system

  'done)

(define (install-rectangular-package)
  ; other package procedures
  
  (define (magnitude z)
    (sqrt (add (square (real-part z))
               (square (imag-part z)))))

  (define (make-from-mag-ang r a)
    (cons (mul r (cosine a)) (mul r (sine a))))

  ; and add interface to this package for the rest system
  'done)

(define (install-polar-package)
  ; other package procedures
  
  (define (real-part z)
    (mul (magnitude z) (cosine (angle z))))

  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))

  (define (make-from-real-imag x y)
    (cons (sqrt (add (square x) (square y)))
          (arctan y x)))

  ; and add interface to this package for the rest system
  'done)

; then we need to make new versions of square, sqrt, sin, cos and artan operations
; square and sqrt can be replaced without apply-generic
(define (square x)
  (mul x x))
(define (sqrt x) 
  (fixed-point (average-damp (lambda (y) (div x y))) 
               1.0))
(define (average x y)
  (div (add x y) 2.0))

; and the rest
(define (sine x)
  (apply-generic 'sine x))
(define (cosine x)
  (apply-generic 'cosine x))
(define (arctan x)
  (apply-generic 'arctan x))

; we`re also asked to add integer and rational realizations to their packages
(define (install-integer-package)
  ; other package procedures
  
  (define (sin-integer x)
    (sin x))
  (define (cos-integer x)
    (cos x))
  (define (artan-integer x y)
    (artan x y))
  
  (put 'sine '(integer) sin-integer)
  (put 'cosine '(integer) cos-integer)
  (put 'arctan '(integer integer) artan-integer)

  'done)

(define (install-rational-package)
  ; other package procedures
  
  (define (sin-rational x)
    (sin (/ (numer x) (denom x))))
  (define (cos-rational x)
    (cos (/ (numer x) (denom x))))
  (define (artan-rational x y)
    (artan (/ (numer x) (denom x))
           (/ (numer y) (denom y))))
  
  (put 'sine '(rational) sin-rational)
  (put 'cosine '(rational) cos-rational)
  (put 'arctan '(rational rational) artan-rational)

  'done)
