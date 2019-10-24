#lang racket

(require sicp)

; If we wouldn't get actual value of procedure
; we will not be able to know
; wether procedure is compound or primitive
; when attempting to apply it to arguments

; That case is obvious with any high-order-procedure

; (map square '(1 2 3)) => (cons (<thunk> 1) (map <thunk> '(2 3)))
