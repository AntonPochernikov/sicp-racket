#lang racket

(require sicp)

(define (>= x y)
  (or (> x y) (= x y)))
(define (square x) (* x x))
(define (prime? n)
  (define (iter counter)
    (cond [(> (square counter) n) true]
          [(= 0 (modulo n counter)) false]
          [else (iter (+ counter 1))]))
  (iter 2))

(define (require pred)
  (if (not pred) (amb)))

(define (list-to-amb L)
  (if (null? L)
      (amb)
      (amb (car L) (list-to-amb (cdr L)))))

(define (prime-sum-pair L1 L2)
  (let* ((first (list-to-amb L1))
         (second (list-to-amb L2)))
    (require (prime? (+ first second)))
    (cons first second)))

(let ((pairs '()))
  (if-fail
    (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
      (permanent-set! pairs (cons p pairs))
      (amb))
     	pairs))

; result of evaluating will be list of pairs with prime sum
; ((8 . 35) (3 . 110) (3 . 20))











