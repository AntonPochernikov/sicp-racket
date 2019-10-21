#lang racket

(require sicp)

; If we wouldn't get actual value of procedure
; we will not be able to know wether procedure is strict in its arguments

(define (square x) (* x x))

(map square '(1 2 3))

; when we are passing procedure as argument it won't be evaluated
