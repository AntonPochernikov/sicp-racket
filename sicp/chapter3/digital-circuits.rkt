#lang sicp

; UTILS
(define (for-each proc L)
  (cond ((null? L) 'done)
        (else
         (begin
           (proc (car L))
           (for-each proc (cdr L))))))
(define (call-each procs)
  (for-each (lambda (f) (f))
            procs))

; QUEUE
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (queue) (cons front-ptr rear-ptr))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "Can not get head of empty queue" front-ptr)
          (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               (queue))
              (else
               (set-cdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)
               (queue)))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with empty queue" (queue)))
            (else
             (set-front-ptr! (cdr front-ptr))
             (queue))))
    (define (print-queue)
      (display front-ptr))

    (define (dispatch m)
      (cond ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'print-queue) (print-queue))
            (else (error "Unknown message -- MAKE-QUEUE" m))))
    dispatch))

(define (insert-queue! q item)
  ((q 'insert-queue!) item))
(define (delete-queue! q)
  ((q 'delete-queue!)))
(define (front-queue q)
  ((q 'front-queue)))
(define (empty-queue? q)
  ((q 'empty-queue?)))

; AGENDA
(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda)
  (car agenda))
(define (segments agenda)
  (cdr agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda)
  (car (segments agenda)))
(define (rest-segments agenda)
  (cdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

(define (remove-first-agenda! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))

  (define (make-new-time-segment)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))

  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr! segments
                        (cons (make-new-time-segment)
                              rest))
              (add-to-segments! rest)))))
  
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments! agenda
                       (cons (make-new-time-segment)
                             segments))
        (add-to-segments! segments))))

(define the-agenda (make-agenda))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda! the-agenda)
        (propagate))))


; WIRE
(define (make-wire)
  (let ((signal 0) (actions '()))
    (define (get-signal) signal)
    (define (set-signal! value)
      (if (not (eq? value signal))
          (begin
            (set! signal value)
            (call-each actions))
          'done))
    (define (add-action! action)
      (set! actions (cons action actions))
      (action))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) get-signal)
            ((eq? m 'set-signal!) set-signal!)
            ((eq? m 'add-action!) add-action!)
            (else
             (error "No method specified for this message -- MAKE-WIRE" m))))
    dispatch))

(define (get-signal wire)
  ((wire 'get-signal)))
(define (set-signal! wire value)
  ((wire 'set-signal!) value))
(define (add-action! wire action)
  ((wire 'add-action!) action))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))


; LOGICAL GATES
(define inverter-delay 3)
(define and-gate-delay 3)
(define or-gate-delay 5)

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
(define (logical-or s1 s2)
  (cond ((or (= s1 1)
             (= s2 1))
         1)
        ((and (= s1 0)
              (= s2 0)) 0)
        (else
         (error "Wrong signal -- LOGICAL-OR" s1 s2))))

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

(define (or-gate o1 o2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal o1) (get-signal o2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! o1 or-action-procedure)
  (add-action! o2 or-action-procedure)
  'ok)

; HALF-ADDER
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    (newline)
    'hald-adder))

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(probe 'carry carry)
(probe 'input-1 input-1)
(probe 'input-2 input-2)

(half-adder input-1 input-2 sum carry)

(set-signal! input-1 1)
(propagate)

(set-signal! input-2 1)
(propagate)

