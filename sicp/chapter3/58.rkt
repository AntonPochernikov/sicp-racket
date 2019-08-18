#lang racket

(require sicp)


(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

; the result will be a floating point represantation of rational number
; that has num as numerator, den as denomerator and radix as a base

; 0 (expand 1 7 10) => 1
; 1 (expand 3 7 10) => 4
; 2 (expand 2 7 10) => 2
; 3 (expand 6 7 10) => 8
; 4 (expand 4 7 10) => 5
; 5 (expand 5 7 10) => 7
; 6 (expand 1 7 10) => 1

; 1 / 7 => 0.1428571...

; 0 (expand 3 8 10) => 3
; 1 (expand 6 8 10) => 7
; 2 (expand 4 8 10) => 5
; 3 (expand 0 8 10) => 0
; 4 (expand 0 8 10) => 0

; 3 / 8 => 0.375










