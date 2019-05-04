#lang sicp

(#%require rackunit)

(define (empty-termlist? term-list) (null? term-list))
(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))

(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (=zero? c) (= c 0))
(define (negate c) (- c))
(define (add c1 c2) (+ c1 c2))
(define (mul c1 c2) (* c1 c2))
(define (div c1 c2) (/ c1 c2))

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

(define (negate-terms terms)
  (if (empty-termlist? terms)
      (the-empty-termlist)
      (let ((first (first-term terms))
            (rest (rest-terms terms)))
        (adjoin-term (make-term (order first)
                                 (negate (coeff first)))
                      (negate-terms rest)))))

(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1)) (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1 (add-terms (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2 (add-terms L1 (rest-terms L2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1) (coeff t2)))
                   (add-terms (rest-terms L1)
                              (rest-terms L2)))))))))

(define (sub-terms L1 L2)
  (add-terms L1 (negate-terms L2)))

(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms L))))))

(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let ((new-c (div (coeff t1) (coeff t2)))
                  (new-o (- (order t1) (order t2))))
              (let ((rest-of-result                       
                        (div-terms (sub-terms L1 
                                              (mul-term-by-all-terms (make-term new-o new-c) 
                                                                     L2))
                                   L2)))
                   (list (adjoin-term (make-term new-o new-c) 
                                      (car rest-of-result)) 
                         (cadr rest-of-result))))))))

(define term-list-1 (list (make-term 5 1) (make-term 3 1) (make-term 0 -1)))
(define term-list-2 (list (make-term 2 1) (make-term 0 -1)))
(define result-1 (list (make-term 3 1) (make-term 1 2)))
(define rest-1 (list (make-term 1 2) (make-term 0 -1)))

; expect (x ** 5 + x ** 3 - 1) / (x ** 2 - 1) to be equal (x ** 3 + 2 * x) with (2 * x - 1) remainder
(check-equal? (div-terms term-list-1 term-list-2) (list result-1 rest-1))

(define (div-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (let ((divided-terms (div-terms (term-list p1)
                                      (term-list p2))))
        (let ((quotient (car divided-terms))
              (remainder (cadr divided-terms)))
          (list (make-poly (variable p1) quotient)
                (make-poly (variable p1) remainder))))
      (error "Polynomials with different variables -- DIV-POLY"
             (list p1 p2))))