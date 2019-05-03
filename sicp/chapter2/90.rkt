#lang sicp

; i will use apply-generic to apply generic operations
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No proper procedure for given types -- APPLY-GENERIC"
           (list op type-tags))))))

; polynomial package
(define (install-polynomial-package)
  (define (make-dense-polynomial variable term-list)
    ((get 'make-polynomial 'dense) variable term-list))
  (define (make-sparse-polynomial variable term-list)
    ((get 'make-polynomial 'sparse) variable term-list))
  
  (define (variable p) (apply-generic 'variable p))
  (define (term-list p) (apply-generic 'term-list p))

  (define (the-empty-termlist) '())
  (define (empty-termlist? term-list) (null? term-list))
  
  (define (first-term term-list) (apply-generic 'first-term p))
  (define (rest-term term-list) (apply-generic 'rest-terms p))
  
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  
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
  ; mul-terms, sub-terms/negate
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-dense-poly (variable p1)
                         (add-terms (term-list p1)
                                    (term-list p2)))
        (error "Polynomials with different variables -- ADD-POLY"
               (list p1 p2))))
  ; sub-poly, mul-poly


  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  ; put sub and mul

  (put 'make-dense-polynomial 'polynomial
       (lambda (var terms) (tag (make-dense-polynomial var terms))))
  (put 'make-sparse-polynomial 'polynomial
       (lambda (var terms) (tag (make-sparse-polynomial var terms))))
  'done)

; dense polynomial package
(define (install-dense-polynomial-package)
  (define (tag-terms terms) (attach-tag 'dense terms))
  
  (define (make-dense-polynomial variable term-list)
    (cons variable (tag-terms term-list)))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define (tag p) (attach-tag 'dense p))
  (put 'make-polynomial 'dense
       (lambda (var terms) (tag (make-poly var terms))))
  (put 'variable 'dense variable)
  (put 'term-list 'dense term-list)

  (define (adjoint-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (put 'adjoint-term 'dense adjoin-term)


  (define (first-term term-list) (car term-list))
  (put 'first-term 'dense first-term)
  (define (rest-term term-list) (cdr term-list))
  (put 'rest-term 'dense rest-term)
  'done)

; sparse polynomial package
(define (install-sparse-polynomial-package)
  (define (tag-terms terms) (attach-tag 'sparse terms))
  
  (define (make-sparse-polynomial variable term-list)
    (cons variable (tag-terms term-list)))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define (tag p) (attach-tag 'sparse p))
  (put 'make-polynomial 'sparse
       (lambda (var terms) (tag (make-poly var terms))))
  (put 'variable 'sparse variable)
  (put 'term-list 'sparse term-list)

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
  (put 'adjoint-term 'sparse adjoin-term)

  (define (first-term term-list)
    (make-term (- (length term-list) 1)
               (car term-list)))
  (put 'first-term 'sparse first-term)
  (define (rest-term term-list) (cdr term-list))
  (put 'rest-term 'sparse rest-term)
  'done)
