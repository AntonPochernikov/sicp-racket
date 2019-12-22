#lang racket

(require sicp)

; SOLUTION

(define (analyze exp)
  (cond [(self-evaluating? exp) (analyze-self-evaluating exp)]
        [(quoted? exp) (analyze-quoted exp)]
        [(variable? exp) (analyze-variable exp)]
        [(assignment? exp) (analyze-assignment exp)]
        [(definition? exp) (analyze-definition exp)]
        [(if? exp) (analyze-if exp)]
        [(lambda? exp) (analyze-lambda exp)]
        [(begin? exp) (analyze-sequence (begin-actions exp))]
        [(cond? exp) (analyze (cond->if exp))]
        [(let? exp) (analyze (let->combination exp))]
        [(amb? exp) (analyze-amb exp)]
        ; adding ramb as a special form
        [(ramb? exp) (analyze-ramb exp)]
        [(application? exp) (analyze-application exp)]
        [else (error "Unknown expression type: ANALYZE" exp)]))

; analyze-ramb utils
(define (length L)
  (if (null? L)
      0
      (+ 1 (length (cdr L)))))

(define (ref n L)
  (cond [(null? L) false]
        [(= n 0) (car L)]
        [else
         (ref (- n 1)
              (cdr L))]))

(define (sample L)
  (let ([index (random (length L))])
    (ref index L)))

(define (filter pred? L)
  (cond [(null? L) '()]
        [(pred? (car L))
         (cons (car L)
               (filter pred? (cdr L)))]
        [else
         (filter pred? (cdr L))]))

(define (without item L)
  (filter (lambda (elem)
            (not (eq? elem item)))
          L))

; analyzing ramb
(define (analyze-ramb exp)
  (let ([cprocs (map analyze (ramb-choices exp))])
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            (let ([current-choice (sample choices)])
              (current-choice
               env
               succeed
               (lambda ()
                 (try-next (without current-choice choices)))))))
      (try-next cprocs))))












