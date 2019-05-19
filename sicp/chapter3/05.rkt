#lang sicp

(define rand-init 0)

(define rand
  (let ((x rand-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (monte-carlo trials experiment)
  (define (iter rest passed)
    (cond ((= rest 0) (/ passed trials))
          ((experiment)
           (iter (- rest 1) (inc passed)))
          (else
           (iter (- rest 1) passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

; solution

(define (estimate-integral predicate x1 x2 y1 y2 trials)
  (define (experiment)
    (predicate ((random-in-range x1 x2)
                (random-in-range y1 y2))))
  (monte-carlo trials experiment))

(define (in-circle r cx cy)
  (lambda (x y)
    (let ((left (+ (square (- x cx))
                   (square (- y cy))))
          (right (square r)))
      (or (< left right)
          (= left right)))))

(define P (in-circle 3 5 7))
