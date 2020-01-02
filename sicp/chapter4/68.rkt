#lang racket

(require sicp)

(rule (append-to-form () ?y ?y))
(rule (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-form ?v ?y ?z))

(rule (reverse? () ()))
(rule (reverse? (?x) (?x)))
(rule (reverse (?first . ?rest) ?reversed)
      (and (append-to-form ?x (?first) ?reversed)
           (reverse ?rest ?x)))











