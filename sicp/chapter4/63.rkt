#lang racket

(require sicp)

; “If S is the son of f,
; and f is the son of G,
; then S is the grandson of G”
(rule (grandson ?S ?G)
      (and (son ?f ?S)
           (son ?G ?f)))

; “If W is the wife of M,
; and S is the son of W,
; then S is the son of M”
(rule (son-of ?S ?M)
      (and (wife ?W ?M)
           (son ?S ?M)))

(grandson ?x Cain)
(son-of ?x Lamech)
(grandson ?x Methushael)









