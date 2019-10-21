#lang racket

(require sicp)

(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value)) 

(define (factorial n)
  (display n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1))

; if we will define unless in applicative-order way
; we will fall into an infinite recursion
; evaluating second argument of unless before applying procedure

; our definition will work in normal-order language






