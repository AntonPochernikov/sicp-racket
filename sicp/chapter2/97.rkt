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

(define (install-scheme-number-package)
  (define (=zero? c) (= c 0))
  (define (negate c) (- c))
  (define (add c1 c2) (+ c1 c2))
  (define (mul c1 c2) (* c1 c2))
  (define (div c1 c2) (/ c1 c2))
  (put '=zero? 'integer =zero?)
  (put 'negate 'integer negate)
  (put 'add '(integer integer) add)
  (put 'mul '(integer integer) mul)
  (put 'div '(integer integer) div)

  (define (tag int) (attach-tag 'integer int))

  (put 'make 'integer
       (lambda (int) (tag int)))
  
  (define (reduce-integers n d)
    (let ((g (gcd n d)))
      (list (/ n g) (/ d g))))
  (put 'reduce '(integer integer) reduce-integers)

  "scheme number package installed")

(define (install-rational-package)
  (define (tag r)
    (attach-tag 'rational r))
  (define (numer x) (car x))
  (define (denom x) (cadr x))
  (define (make-rat n d)
    (reduce n d))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))

  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  
  "rational package installed")

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
           (make-term (add (order t1) (order t2))
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

  (define (pseudo-remainder-terms L1 L2) 
    (let ((o1 (order (first-term L1)))
          (o2 (order (first-term L2)))
          (c (coeff (first-term L2))))
      (let ((multiplier (expt c (+ 1 (- o1 o2)))))
        (let ((multiplied-terms (adjoin-term
                        (make-term 0 multiplier)
                        (the-empty-termlist))))
          (cadr (div-terms (mul-terms multiplied-terms L1) L2))))))

  (define (gcd-coeff terms)
    (define (coeff-list terms)
      (if (empty-termlist? terms)
          '()
          (cons (coeff (first-term terms))
                (coeff-list (rest-terms terms)))))
    (apply gcd (coeff-list terms)))
  
  
  (define (gcd-terms L1 L2)
    (if (empty-termlist? L2)
        (let ((coeff-terms
               (adjoin-term (make-term 0 (gcd-coeff L1))
                            (the-empty-termlist))))
          (car (div-terms L1 coeff-terms)))
        (gcd-terms L2 (pseudo-remainder-terms L1 L2))))

  (define (gcd-polys p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (tag (make-poly (variable p1)
                        (gcd-terms (term-list p1) (term-list p2))))
        (error "Polynomials with different variables -- GCD-POLYS"
               (list p1 p2))))
  
  (put 'greatest-common-divisor '(polynomial polynomial) gcd-polys)

  ; solution
  (define (int-factor n d)
    (let ((o1 (order (first-term n)))
          (o2 (order (first-term d)))
          (c (coeff (first-term d))))
      (make-term 0
                 (expt c (+ 1 (- o1 o2))))))

  (define (reduce-terms n d)
    (let ((gcd (gcd-terms n d))
          (k (int-factor n d)))
        (let ((nn (mul-term-by-all-terms k n))
              (dd (mul-term-by-all-terms d n)))
          (let ((gcd-2 (gcd-terms nn dd)))
            (list (div-terms nn gcd-2)
                  (div-terms dd gcd-2))))))
  
  (define (reduce-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((var (variable p1))
              (terms (reduce-terms (term-list p1) (term-list p2))))
          (list (make-poly var (car terms))
                (make-poly var (cadr terms))))
        (error "Polynomials with different variables -- GCD-POLYS"
               (list p1 p2))))
  (put 'reduce '(polynomial polynomial) reduce-poly)
    

  "polynomial package installed")

(install-scheme-number-package)
(install-rational-package)
(install-polynomial-package)

(define (add arg1 arg2)
  (apply-generic 'add arg1 arg2))
(define (mul arg1 arg2)
  (apply-generic 'mul arg1 arg2))
(define (div arg1 arg2)
  (apply-generic 'div arg1 arg2))
(define (=zero? arg)
  (apply-generic '=zero? arg))
(define (negate arg)
  (apply-generic 'negate arg))
(define (reduce arg1 arg2)
  (apply-generic 'reduce arg1 arg2))
(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (make-poly var terms)
  ((get 'make 'polynomial) var terms))
(define (greatest-common-divisor p1 p2)
  (apply-generic 'greatest-common-divisor p1 p2))
