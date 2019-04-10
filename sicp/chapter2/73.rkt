#lang sicp

; we are getting appropriate procedure for given operation

; number? and variable? predicates are guard expressions that doesn`t actually contain any operation type

(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (power base exp)
  (* exp (* base (log exp))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 0 1))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (install-sum-product-package)
  (define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
  (define (addend s) (car s))
  (define (augend s) (cadr s))

  (define (sum-deriv exp)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (define (multiplier p) (cadr p))
  (define (multiplicand p) (caddr p))
  (define (product-deriv exp)
    (make-sum
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp))))
  (put 'deriv '+ sum-deriv)
  (put 'deriv '* product-deriv)

  ; add exponention package
  (define (make-exponention base exp)
    (cond ((=number? exp 0) 1)
          ((=number? exp 1) base)
          ((and (number? exp) (number? base)) (power base exp))
          (else (list '** base exp))))
  (define (exponention? exp)
    (and (pair? exp) (eq? (car exp) '**)))
  (define (base exp)
    (cadr exp))
  (define (exponent exp)
    (caddr exp))
  
  (define (exponention-deriv exp)
    (make-product (make-product (exponent exp)
                                (make-exponention (base exp)
                                                  (- (exponent exp) 1)))
                  (deriv (base exp) var)))

  (put 'deriv '** exponention-deriv)
  'done)

; ((get (operator exp) 'deriv) (operands exp) var)
; in this scenario we will have to switch operands of put procedure
