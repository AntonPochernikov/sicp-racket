#lang sicp

(#%require rackunit)

(define (make-monitored f)
  (let ((calls 0))
    (define (reset-count)
      (set! calls 0)
      (display "Calls have been reset")
      calls)
    (define (call arg)
      (set! calls (inc calls))
      (f arg))
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls?) calls)
            ((eq? m 'reset-count) (reset-count))
            (else (call m))))
    dispatch))

(define s (make-monitored sqrt))
(check-equal? (s 100) 10)
(check-equal? (s 64) 8)
(check-equal? (s 'how-many-calls?) 2)

(define i (make-monitored identity))
(check-equal? (i 5) 5)
(check-equal? (i 'how-many-calls?) 1)
