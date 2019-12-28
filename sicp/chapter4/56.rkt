#lang racket

(require sicp)

; a)
(and (supervisor ?person (Ben Bitdiddle))
     (address ?person ?address))

; b)
(and (salary (Ben Bitdiddle) ?max-amount)
     (salary ?person ?salary)
     (lisp-value > ?max-amount ?salary))

; c)
(and (supervisor ?person ?supervisor)
     (not (job ?supervisor (computer . ?position)))
     (job ?supervisor ?position))

















