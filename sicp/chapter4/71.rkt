#lang racket

(require sicp)

; The main reason we use delay here is that our
; stream-append and interleave procedures are not special forms.
; That means its arguments are going to be
; be evaluated before applying. This can cause trouble.

; Recalling the example with Mickey and Minnie.
(assert! (married Minnie Mickey))
(assert! (rule (married ?x ?y)
               (married ?y ?x)))

; This causes infinite loop if we'll try to find following assertions
; (married ?x ?y)

; Delaying (apply-rules query-pattern frame) will prevent infinite loops.









