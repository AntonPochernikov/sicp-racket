#lang racket

(require sicp)

; I will use "last pair" rule from exercise 4.62
; to determine wether grandson is the last item in relation list
(rule (last-pair (?x) (?x)))
(rule (last-pair (?x . ?y) (?z))
      (last-pair ?y (?z)))

(rule ((grandson) ?x ?y)
      (grandson ?x ?y))
(rule ((great . ?relation) ?parent ?child)
      (and (last-pair ?relation (grandson))
           (son ?parent ?x)
           (?relation ?x ?child)))







