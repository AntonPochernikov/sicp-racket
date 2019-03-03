#lang sicp

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (cube x) (* x x x))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b) dx))



(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (g k)
    (f (+ a (* k h))))
  (define (term k)
    (* (cond ((odd? k) 4)
             ((or (= k 0) (= k n)) 1)
             ((even? k) 2))
       (g k)))
  (/ (* h (sum term 0 inc n))
     3))

(display "integral cube 0 1 0.01\n=> ")
(integral cube 0 1 0.01)
(display "simpson cube 0 1 100.0\n=> ")
(simpson cube 0 1 100.0)

(display "integral cube 0 1 0.001\n=> ")
(integral cube 0 1 0.001)
(display "simpson cube 0 1 1000.0\n=> ")
(simpson cube 0 1 1000.0)