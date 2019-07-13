#lang sicp

(define (for-each proc L)
  (cond ((null? L) #t)
        (else
         (begin
           (proc (car L))
           (for-each proc (cdr L))))))

(define (make-wire)
  (let ((signal 0)
        (actions '()))
    (define (get-signal) signal)
    (define (set-signal! value)
      (begin
        (set! signal value)
        (for-each (lambda (f) (f) actions))))
    (define (add-action! action)
      (set! actions (cons action actions)))
    (define (dispatch m)
      (cond ((= m 'get-signal) (get-signal))
            ((= m 'set-signal!) set-signal!)
            ((= m 'add-action!) add-action!)
            (else
             (error "No method specified for this message -- MAKE-WIRE" m))))
    dispatch))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire value)
  ((wire 'set-signal!) value))
(define (add-action! wire action)
  ((wire 'add-action!) action))

(define (after-delay delay proc) (proc))
(define inverter-delay 100)
(define and-gate-delay 100)
(define or-gate-delay 100)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else
         (error "Wrong signal -- LOGICAL-NOT" s))))
(define (logical-and s1 s2)
  (cond ((and (= s1 1)
              (= s2 1))
         1)
        ((or (= s1 0)
             (= s2 0)) 0)
        (else
         (error "Wrong signal -- LOGICAL-AND" s1 s2))))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

; SOLUTION
(define (or-gate o1 o2 output)
  (define w1 (make-wire))
  (define w2 (make-wire))
  (define w3 (make-wire))
  (inverter o1 w1)
  (inverter o2 w2)
  (and-gate w1 w2 w3)
  (inverter w3 output)
  'ok)

; or-gate-delay-time = inverter-delay + and-delay