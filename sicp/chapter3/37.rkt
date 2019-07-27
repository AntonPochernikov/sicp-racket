#lang racket

(require sicp)
(require rackunit)

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (make-connector)
  (let ((value false)
        (informant false)
        (constraints '()))
    (define (has-value?)
      (if informant true false))
    (define (set-value! newval setter)
      (cond ((not (has-value?))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value! retractor)
      (if (eq? retractor informant)
          (begin (set! value false)
                 (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (get-value) value)
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints)))
      (if (has-value?)
          (inform-about-value new-constraint))
      'done)

    (define (me request)
      (cond ((eq? request 'has-value?) has-value?)
            ((eq? request 'get-value) get-value)
            ((eq? request 'set-value!) set-value!)
            ((eq? request 'forget-value!) forget-my-value!)
            ((eq? request 'connect) connect)
            (else
             (error "Unknown operation: CONNECTOR"
                    request))))
    me))

(define (has-value? connector)
  ((connector 'has-value?)))
(define (set-value! connector newval setter)
  ((connector 'set-value!) newval setter))
(define (get-value connector)
  ((connector 'get-value)))
(define (forget-value! connector retractor)
  ((connector 'forget-value!) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1)
                          (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum)
                          (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum)
                          (get-value a2))
                       me))))
  (define (forget-value)
    ; only values that were set by this constraint are actually lost
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))

  (define (me request)
    (cond ((eq? request 'got-value) process-new-value)
          ((eq? request 'lose-value) forget-value)
          (else
           (error "Unknown request: ADDER"
                  request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (eq? (get-value m2) 0))
               (and (has-value? m2) (eq? (get-value m1) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1)
                          (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product)
                          (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product)
                          (get-value m2))
                       me))))
  (define (forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'got-value) process-new-value)
          ((eq? request 'lose-value) forget-value)
          (else
           (error "Unknown request: MULTIPLIER"
                  request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (divider divisible divisor quotient)
  (define (process-new-value)
    (cond ((and (has-value? divisible) (eq? (get-value divisor) 0))
           (error "Divisor equaled 0: DIVIDER" divisor))
          ((and (has-value? divisor) (eq? (get-value quotient) 0))
           (set-value! divisible 0 me))
          ((and (has-value? divisible) (has-value? divisor))
           (set-value! quotient
                       (/ (get-value divisible)
                          (get-value divisor))
                       me))
          ((and (has-value? quotient) (has-value? divisible))
           (set-value! divisor
                       (/ (get-value divisible)
                          (get-value quotient))
                       me))
          ((and (has-value? quotient) (has-value? divisor))
           (set-value! divisible
                       (* (get-value divisor)
                          (get-value quotient))
                       me))))
  (define (forget-value)
    (forget-value! quotient me)
    (forget-value! divisible me)
    (forget-value! divisor me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'got-value) process-new-value)
          ((eq? request 'lose-value) forget-value)
          (else
           (error "Unknown request: MULTIPLIER"
                  request))))
  (connect divisible me)
  (connect divisor me)
  (connect quotient me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request: CONSTANT"
           request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (inform-about-value constraint)
  ((constraint 'got-value)))
(define (inform-about-no-value constraint)
  ((constraint 'lose-value)))

(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'got-value) process-new-value)
          ((eq? request 'lose-value) process-new-value)))
  (connect connector me)
  me)

; SOLUTION
(define (c+ a1 a2)
  (let ((sum (make-connector)))
    (adder a1 a2 sum)
    sum))
(define (c* m1 m2)
  (let ((product (make-connector)))
    (multiplier m1 m2 product)
    product))
(define (c/ divisible divisor)
  (let ((quotient (make-connector)))
    (divider divisible divisor quotient)
    quotient))
(define (cv value)
  (let ((const (make-connector)))
    (constant value const)
    const))

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

; TESTS
(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

(set-value! F 212 'user)
(check-equal? (get-value C) 100)

(forget-value! F 'user)
(set-value! C 100 'user)
(check-equal? (get-value F) 212)




















