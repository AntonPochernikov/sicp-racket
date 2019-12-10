#lang racket

(require sicp)

(define (an-integer-starting-from i)
  (amb i (an-integer-starting-from (+ i 1))))

(define (a-pythogorean-triple)
  (let ([i (an-integer-starting-from 1)])
    (let ([j (an-integer-starting-from i)])
      (let ([k (an-integer-starting-from j)])
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

; The crucial thing is that our search strategy is depth-first search.
; It means that we won't ever get any changes of 'i' and 'y' values.
; This causes infinite recursion since the only triple that is pythogorean in this case will be (1, 1, 2).
; After that we either hit the barier of infinite searches with no proper result
; or rather get to the point when 'k' becomes bigger than maximum possible value of number in our language.

(define (an-integer-between low high)
  (require (> high low))
  (amb low
       (an-integer-between (+ low 1)
                           high)))

(define (a-pythogorean-triple from n)
  (let ([i (an-integer-starting-from n)])
    (let ([j (an-integer-between i (square i))])
      (let ([k (an-integer-starting-from (square j))])
        (require (>= (j i)))
        (require (>= (k j)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))




