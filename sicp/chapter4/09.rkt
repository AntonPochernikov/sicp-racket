#lang racket

; (for start predicate increment body))
(define (make-for init predicate increment body)
  (list 'for init predicate increment body))

(define (for-init exp) (cadr exp))
(define (for-predicate exp) (caddr exp))
(define (for-increment exp) (cadddr exp))
(define (for-body exp) (car (cddddr exp)))

(define (for->combination exp)
  (let ([init (for-init exp)]
        [predicate (for-predicate exp)]
        [increment (for-increment exp)]
        [body (for-body exp)]))
  (define (loop i)
    (if (predicate i)
        (cons body (loop (increment i) body))
        'done))
  (make-begin
   (loop init)))

(put 'eval 'for (lambda (exp env) (eval (for->combination exp) env)))














