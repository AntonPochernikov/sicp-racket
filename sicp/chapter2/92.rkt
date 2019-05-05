#lang sicp

(define (poly? p) (= (type-tag poly) 'polynomial))

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

(define (same-variable? v1 v2) (eq? v1 v2))

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

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

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms L))))))

(define (find-same-var p1 p2)
  (define (get-vars acc rest)
    (cond ((empty-termlist? rest) acc)
          ((poly (first-term rest))
           (get-vars (cons (first-term rest) acc)
                     (rest-terms rest)))
          (else (get-vars acc (rest-terms rest)))))
  (let ((vars-1 (get-vars '() (terms-1 (term-list p1))))
        (vars-2 (get-vars '() (terms-2 (term-list p2)))))
    ; find same-variables in terms as intersection of variable lists
    (let ((same-variables (intersection vars-1 vars-2)))
      ; if we will not get same-variables we can take first variable and adjust second poly
      (if (null? same-variables)
          (variable p1)
          (car same-variables)))))

(define (adjust-poly-to-var poly var)
  (if (eq? (variable poly) var)
      poly
      (let ((terms (term-list poly)))
        ; adjust all polynomial terms
        (define (adjust-terms acc rest)
          (if (empty-termlist? rest)
              acc
              (let ((current (first-term rest)))
                (if (poly? (coeff current))
                    (if (same-variable? (variable (coeff current)) var)
                        (let ((new-term
                               (make-term 0
                                          (mul-term-by-all-terms (make-term (order current) (variable poly))
                                                                 (term-list (coeff current))))))
                          (adjust-terms (adjoin-term new-term acc) (rest-terms rest)))
                        (let ((new-term
                               (make-term 0
                                          (adjust-poly-to-var (coeff current)
                                                              var))))
                          (adjust-terms (adjoin-term new-term acc) (rest-terms rest))))
                    (let ((new-term
                           (make-term 0
                                      (make-poly var
                                                 (adjoin-term (make-term (order current) (coeff current))
                                                              (the-empty-termlist))))))
                      (adjust-terms (adjoin-term new-term acc) (rest-terms rest)))))))
        ; make new polynomial with adjusted termlist
        (make-poly var
                   (adjust-terms (the-empty-termlist) terms)))))

(define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (let ((same-var (find-same-var p1 p2)))
        (make-poly same-var
                   (add-terms (term-list (adjust-poly-to-var p1))
                              (term-list (adjust-poly-to-var p2)))))))

(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))

(define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (let ((same-var (find-same-var p1 p2)))
        (make-poly same-var
                   (mul-terms (term-list (adjust-poly-to-var p1))
                              (term-list (adjust-poly-to-var p2)))))))
