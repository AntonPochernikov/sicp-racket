#lang racket

(require sicp)

; this way we can log time spent on evaluation
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ([input (read)])
    ; save timestamps before and after the evaluation
    (let ([start-time (current-inexact-milliseconds)]
          [output (eval input the-global-environment)]
          [end-time (current-inexact-milliseconds)])
      (display "Execution time:")
      (display (- end-time start-time))
      (display "ms")
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

; i will try to do test simple recursive factorial function
; here are the results of (fact 10000) for first interpreter
; : ~257ms
; interpreter with analyzing
; : ~146ms

; let's dive in tree recursion with fibs procedure
(define (fib n)
  (if (< n 3)
      1
      (+ (fib (- n 1))
         (fib (- n 2)))))
; (fib 30) takes
; 11756ms
; versus
; 6263ms

; almost 2-times improvement







