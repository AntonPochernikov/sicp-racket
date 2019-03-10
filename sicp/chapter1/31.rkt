#lang sicp

(#%require rackunit)

(define (product term a next b)
  (define (iter n result)
    (if (> n b)
        result
        (iter (next n) (* (term n) result))))
  (iter a 1))

(define (product-recursive term a next b)
  (if (> a b)
      1
      (* (term a) (product-recursive term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

(check-equal? (factorial 5) 120)

(define (pi approximation)
  (define (term n)
    (/ (if (even? n) (+ n 2) (+ n 1))
       (if (even? n) (+ n 1) (+ n 2))))
  (* (product term 1 inc approximation) 4.0))

(define (pi-recursive approximation)
  (define (term n)
    (/ (if (even? n) (+ n 2) (+ n 1))
       (if (even? n) (+ n 1) (+ n 2))))
  (* (product-recursive term 1 inc approximation) 4.0))

(check-equal? (pi 1) (pi-recursive 1))
(check-equal? (pi 10) (pi-recursive 10))
(check-equal? (pi 100) (pi-recursive 100))
(check-equal? (pi 1000) (pi-recursive 1000))

(display "1 => ")
(display (pi 1))
(display "\n")
(display "10 => ")
(display (pi 10))
(display "\n")
(display "100 => ")
(display (pi 100))
(display "\n")
(display "1000 => ")
(display (pi 1000))
(display "\n")