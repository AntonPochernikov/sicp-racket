#lang racket

(require sicp)

; This behavior is happen cause we check if person is same
; only for current pair.

(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (not (same ?person-1 ?person-2))))

; We can make it if we will compare their names and
; and show only the pair where biggest one is first.
(define (list-to-string L)
  (if (null? L)
      ""
      (string-append (symbol->string (car L))
                     (list-to-string (cdr L)))))

(define (person>? p1 p2)
  (string>? (list-to-string p1)
            (list-to-string p2)))



(rule (leave-near-uniq ?person-1 ?person-2)
      (lives-near ?person-1 ?person-2)
      (person>? ?person-1 ?person-2))








