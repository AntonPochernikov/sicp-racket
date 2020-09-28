#lang racket

; A)
; Since val contains Fib(n - 2) we don't need to assign
; val to n to restore val and have both Fib(n - 2) and Fib(n - 1)
; We can restore Fib(n - 1) from stack to n.
 afterfib-n-2 ; upon return, val contains Fib(n - 2)
 (assign n (reg val)) ; n now contains Fib(n - 2)
 (restore val) ; val now contains Fib(n - 1)

; B)
(define (make-save instruction machine stack pc)
  (let ([reg-name (stack-inst-reg-name instruction)])
    (let ([register (get-register machine reg-name)])
      (lambda ()
        (push
         stack
         (list reg-name (get-contents register)))
        (advance-pc pc)))))

(define (make-restore instruction machine stack pc)
  (let ([reg-name (stack-inst-reg-name instruction)])
    (let ([register (get-register machine reg-name)])
      (lambda ()
        (let ([contents (pop stack)])
          (if (eq? (car contents) reg-name)
              (begin
                (set-contents! register (cadr contents))
                (advance-pc pc))
              (error
               "Value is not from the register: "
               (car contents))))))))

; C)
; Let's create a table of stack in make-new-machine procedure
(define (make-new-machine)
  (let ([program-counter (make-register 'program-counter)]
        [flag (make-register 'flag)]
        [stacks (list)] ; make stack on every allocations
        [the-instruction-sequence '()])
    (let ([the-operations
           (list (list 'initialize-stack ; initialize all stacks
                       (lambda ()
                         (map
                          (lambda (stack-pair)
                            (initialize (cadr stack-pair)))
                          stacks))))]
          [register-table
           (list (list 'program-counter program-counter)
                 (list 'flag flag))])
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Register already declared: " name)
            (begin
              (set! register-table
                    (cons (list name (make-register name))
                          register-table))
              (set! stacks ; save stack and its reg-name
                    (cons (list name (make-stack))
                          stacks))))
        'register-allocated)
      (define (lookup-register name)
        (let ([value (assoc name register-table)])
          (if value
              (cadr value)
              (error "Unknown register: " name))))
      (define (execute)
        (let ([instructions (get-contents program-counter)])
          (if (null? instructions)
              'executed
              (begin
                ((instruction-execution-proc (car instructions)))
                (execute)))))
      (define (dispatch message)
        (cond [(eq? message 'start)
               (set-contents! program-counter
                              the-instruction-sequence)
               (execute)]
              [(eq? message 'install-instruction-sequence)
               (lambda (seq)
                 (set! the-instruction-sequence seq))]
              [(eq? message 'allocate-register)
               allocate-register]
              [(eq? message 'get-register)
               lookup-register]
              [(eq? message 'install-operations)
               (lambda (ops)
                 (set! the-operations
                       (append the-operations ops)))]
              [(eq? message 'stack) stacks]
              [(eq? message 'operations) the-operations]
              [else
               (error "Unknown message: MACHINE" message)]))
      dispatch)))

(define (make-save instruction machine stacks pc)
  (let* ([reg-name (stack-inst-reg-name instruction)]
         [register (get-register machine reg-name)]
         [stack (cadr (assoc reg-name stacks))])
    (lambda ()
      (push stack (get-contents register))
      (advance-pc pc))))

(define (make-restore instruction machine stacks pc)
  (let* ([reg-name (stack-inst-reg-name instruction)]
         [register (get-register machine reg-name)]
         [stack (cadr (assoc reg-name stacks))])
    (lambda ()
      (set-contents! register (pop stack))
      (advance-pc pc))))

; The alternative would be to make a separate stack inside every register


