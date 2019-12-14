#lang racket

(require sicp)

; Changing the order of restrictions doesn't affect the answer at all.

; However things might be different with evaluation time.
; I've seen this explanation in internet though.
; Let's assume that we have following restrictions

; 1) distinct?
; 2) cooper 1

; First restrictions will be tested 5 ** 5 = 3125 times
; and will produce 120 correct results
; This means that we have 3125 * R1 + 120 * R2 evaluation times

; If we switch the order of this restrictions we will get
; 3125 times of R2 that will produce 2500 correct answers
; This means that we have 3125 * R2 + 2500 * R1
; As you can see we can compare 625 * R1 and 3005 * R2
; and assume that if we have same operation on testing
; this can cause a far better performance.




