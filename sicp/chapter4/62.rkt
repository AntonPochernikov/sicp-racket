#lang racket

(require sicp)

(rule (last-pair (?x) (?x)))
(rule (last-pair (?x . ?y) (?z))
      (last-pair ?y (?z)))

; I do not have logic interpreter in my hands
; but i'm pretty sure i would get following results:
; (last-pair (3) ?x) => (3)
; (last-pair (1 2 3) ?x) => (3)
; (last-pair ?x (3)) => infinite recursion









