#lang racket

(define x 10)
(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))

; P1 operations: a) access x1, b) access x2, c) set x to x1 * x2
; P2 operations: d) access x3, e) access x4, f) access x5, g) set x to x3 * x4 * x5

; P1 => P2: x1 = 10, x2 = 10, set x to 100, x3 = 100, x4 = 100, x5 = 100, set x to 1,000,000
; P2 => P1: x3 = 10, x4 = 10, x5 = 10, set x to 1000, x1 = 1000, x2 = 1000, set x to 1,000,000
; d e f a g b c => x3 = 10, x4 = 10, x5 = 10, x1 = 10, set x to 1000, x2 = 1000, set x to 10,000
; a b d c e f g => x1 = 10, x2 = 10, x3 = 10, set x to 100, x4 = 100, x5 = 100 set x to 100,000
; a b d e c f g => x1 = 10, x2 = 10, x3 = 10, x4 = 10, set x to 100, x5 = 100, set x to 10,000
; a b d e f g c => x1 = 10, x2 = 10, x3 = 10, x4 = 10, x5 = 10, set x to 1000, set x to 100
; d e f a b c g => x3 = 10, x4 = 10, x5 = 10, x1 = 10, x2 = 10, set x to 100, set x to 1,000

(define s (make-serializer))
(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (* x x x)))))

; in that case we can only get P1 => P2 or P2 => P1, value will be set to 1,000,000
