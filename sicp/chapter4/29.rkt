#lang racket

(define (fib n)
  (if (< n 3)
      1
      (+ (fib (- n 1))
         (fib (- n 2)))))

; without memoization
; (fib 20) => 404ms
; (fib 25) => 5625ms

; with memoization
; (fib 20) => 100ms
; (fib 25) => 1050ms

; This simple fibonacci program would be
; much slower without memoization.

(define count 0)
(define (id x) (set! count (+ count 1)) x)

(define (square x) (* x x))

;;; L-Eval input:
; (square (id 10))

;;; L-Eval value:
; 100

; with memo
;;; L-Eval input:
; count

;;; L-Eval value:
; 1

; without memo
;;; L-Eval input:
; count

;;; L-Eval value:
; 2















