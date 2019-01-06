#lang racket/base

(require rackunit)

(define (square x) (* x x))
(check-equal? (square 4) 16)
(check-equal? (square -3) 9)
(check-equal? (square (+ 1 2 3)) 36)

(define (findSmallest a b c)
  (cond
    ((and (< a b) (< a c)) a)
    ((and (< b a) (< b c)) b)
    (else c)
  )
)
(check-equal? (findSmallest 1 -2 3) -2)
(check-equal? (findSmallest 7 7 7) 7)

(define (sumOfSquares x y) (+ (square x) (square y)))
(check-equal? (sumOfSquares 3 -2) 13)
(check-equal? (sumOfSquares (square 2) (findSmallest 7 6 5)) 41)

(define (sumOfBiggestSquares a b c)
  (cond
    ((= (findSmallest a b c) a) (sumOfSquares b c))
    ((= (findSmallest a b c) b) (sumOfSquares a c))
    (else (sumOfSquares a b))
  )
)
(check-equal? (sumOfBiggestSquares 4 2 -3) 20)
(check-equal? (sumOfBiggestSquares (sumOfBiggestSquares 1 2 -2) 2 -3) 29)