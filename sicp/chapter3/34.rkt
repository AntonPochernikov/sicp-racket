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

(define (celcius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

; SOLUTION
(define (squarer a b)
  (multiplier a a b))

(define input (make-connector))
(define output (make-connector))

(probe "squarer" output)
(constant 5 input)
(squarer input output)

(define input-2 (make-connector))
(define output-2 (make-connector))

(probe "sqrt" input-2)
(constant 25 output-2)
(squarer input-2 output-2)

; although squarer will perfectly work for output connector
; we won't get the input value with an output value set
; due to multiplier implementation

; output of squarer can be easily set to negative number
; that is inappropriate in case of squares


















