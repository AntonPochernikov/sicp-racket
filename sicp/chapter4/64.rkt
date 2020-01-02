#lang racket

(require sicp)

(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          ; The main difference is that we swapped and conclusions.
          ; It causes recursive application of the rule "outranked-by",
          ; since ?middle-manager and ?staff-person could be the same.
          (and (outranked-by ?middle-manager ?boss)
               (supervisor ?staff-person ?middle-manager))))











