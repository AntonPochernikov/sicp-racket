#lang sicp

(define (equ? x y)
  (apply-generic 'equ? x y))

(define (apply-generic op . args)
  ; here we are just dropping all arguments from the beginning
  (let ((dropped (drop-all args)))
    (let ((proc (get op (map type-tag dropped))))
      (if proc
          (apply proc (map contents dropped))
          (error "operation not found -- APPLY-GENERIC" op args)))))

(define (project obj)
  (apply-generic 'project obj))

(define type-tower '(integer rational real complex))
; this drop implementation will only work for type-tower above
(define (drop obj)
  (cond ((eq? (type-tag obj) 'integer) obj)
        ((equ? (obj (raise (project obj))))
         (drop (project obj)))
        (else obj)))

(define (drop-all objects)
  (map drop objects))

(define (complex->real complex)
  (make-real (real-part complex)))
; drop procedure will check if result of raised and projected value is equal to derived one
(put 'project '(complex) complex->real)

(define (real->rational real)
  (make-rational (numerator real) (denominator real)))
; using built-in scheme primitives to construct rational number from real one
(put 'project '(real) real->rational)

(define (rational->integer rational)
  (numer rational))
; here is a thing that we are not able to drop raional number if its denom is not 1
; we will check if its true in drop procedure
(put 'project '(rational) rational->integer)
