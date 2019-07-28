#lang racket

(define x 10)
(define s (make-serializer))
(parallel-execute (lambda () (set! x ((s (lambda () (* x x))))))
                  (s (lambda () (set! x (+ x 1)))))

; SOLUTION
; 101: P1 sets x to 100, then P2 sets it to 100
; 121: P2 sets x to 11, then P1 sets it to 121,
; 100: P1 accesses x (10), then P2 set it to 11, then P1 sets it to 100
