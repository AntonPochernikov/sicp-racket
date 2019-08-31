#lang racket

; implementation in excercise 3.63 will create new stream on every iteration
; that's why our memoized delay procedure won't reduce computations of stream-cdr

; if there would be no memo optimization, it will be not important
; which defenition to use
