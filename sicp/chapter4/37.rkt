#lang racket

(require sicp)

(define (a-pythogorean-triple-between low high)
  (let ([i (an-integer-between low high)]
        [hsq (* high high)])
    (let ([j (an-integer-between i high)])
      (let ([ksq (+ (* i i) (* j j))])
        (require (>= hsq ksq))
        (let ([k (sqrt ksq)])
          (require (integer? k))
          (list i j k))))))

; SOLUTION
; With that solution we do not have third ambiguous element
; so our tree will be one layer less deep.
; This filter (require (>= hsq ksq))
; will cut out all irrelevant i and j elements.






















