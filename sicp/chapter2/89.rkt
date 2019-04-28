#lang sicp

(define (install-polynomial-package)
  ; rest operations

  ; we will have to change first-term selector and adjoin-term proc
  (define (first-term term-list)
    (make-term (- (lenght term-list) 1)
               (car term-list)))

  (define (adjoin-term term term-list)
    (cond ((=zero? term) term-list)
          ((< (+ (order term) 1) (length term-list))
           (cons (coeff (first-term term-list))
                 (adjoin-term term (rest-terms term-list))))
          ((> (+ (order term) 1) (length term-list))
           (cons (coeff term)
                 (adjoin-term (make-term (- (order term) 1)
                                         0)
                              (rest-terms term-list))))
          (else (+ (order term) 1) (length term-list))
           (cons (add (coeff term) (coeff (first-term term-list)))
                 (rest-terms term-list))))

  'done)
