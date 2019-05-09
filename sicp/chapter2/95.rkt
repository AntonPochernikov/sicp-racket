#lang sicp

(#%require rackunit)

(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))

(define (attach-tag tag contents)
  (list tag contents))
(define (type-tag arg)
  (car arg))
(define (contents arg)
  (cadr arg))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "Method not found -- APPLY-GENERIC"
           (list op type-tags))))))

(define (install-polynomial-package)
  (define (tag p) (attach-tag 'polynomial p))
  (define (make-poly variable term-list)
    (list variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cadr p))

  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))

  (define (same-variable? v1 v2) (eq? v1 v2))
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

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polynomials with different variables"
               (list p1 p2))))

  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))

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

  ; polynomial operations
  ; ...

  ; solution
  (define (remainder-terms t1 t2)
    (cadr (div-terms t1 t2)))

  (define (gcd-terms a b)
    (if (empty-termlist? b)
        a
        (gcd-terms b (remainder-terms a b))))

  (define (gcd-polys p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (tag (make-poly (variable p1)
                                           (gcd-terms (term-list p1) (term-list p2))))
        (error "Polynomials with different variables -- GCD-POLYS"
               (list p1 p2))))
  ; add 
  (put 'greatest-common-divisor '(polynomial polynomial) gcd-polys)

  "polynomial package installed")

(install-polynomial-package)

(define (make-poly var terms)
  ((get 'make 'polynomial) var terms))
(define (greatest-common-divisor p1 p2)
  (apply-generic 'greatest-common-divisor p1 p2))
(define (mul v1 v2)
  (apply-generic 'mul v1 v2))

(define p1 (make-poly 'x '((2 1) (1 -2) (0 1))))
(define p2 (make-poly 'x '((2 11) (0 1))))
(define p3 (make-poly 'x '((1 13) (0 5))))

(define q1 (mul p1 p2))
(define q2 (mul p1 p3))
(display (greatest-common-divisor q1 q2))