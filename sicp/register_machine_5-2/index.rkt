#lang sicp

; machine model
(define (make-machine register-names
                      operations
                      controller-instructions)
  (let ([machine (make-new-machine)])
    (for-each
     (lambda (register-name)
       ((machine 'allocate-register) register-name))
     register-names)
    ((machine 'install-operations) operations)
    ((machine 'install-instruction-sequence)
     (assemble controller-instructions machine))
    machine))

; registers
(define (make-register name)
  (let ([contents '*unassigned*])
    (define (dispatch message)
      (cond [(eq? message 'get) contents]
            [(eq? message 'set)
             (lambda (value) (set! contents value))]
            [else
             (error "Unknown request: REGISTER" message)]))
    dispatch))
(define (get-contents register) (register 'get))
(define (set-contents! register value)
  ((register 'set) value))

; stack
(define (make-stack)
  (let ([s '()])
    (define (push x) (set! s (cons x s)))
    (define (pop)
      (if (null? s)
          (error "Empty stack: POP")
          (let ([top (car s)])
            (set! s (cdr s))
            top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (empty?)
      (eq? s '()))
    (define (dispatch message)
      (cond [(eq? message 'push) push]
            [(eq? message 'pop) (pop)]
            [(eq? message 'empty?) (empty?)]
            [(eq? message 'initialize) (initialize)]
            [else
             (error "Unknown message: STACK" message)]))
    dispatch))

(define (pop stack) (stack 'pop))
(define (push stack value) ((stack 'push) value))
(define (initialize stack) (stack 'initialize))
(define (empty-stack? stack) (stack 'empty?))

; make-new-machine
(define (make-new-machine)
  (let ([program-counter (make-register 'program-counter)]
        [flag (make-register 'flag)]
        [stack (make-stack)]
        [the-instruction-sequence '()])
    (let ([the-operations
           (list (list 'initialize-stack
                       (lambda () (initialize stack))))]
          [register-table
           (list (list 'program-counter program-counter)
                 (list 'flag flag))])
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Register already declared: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
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
                ((instruction-execution-proc (car insctructions)))
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
              [(eq? message 'stack) stack]
              [(eq? message 'operations) the-operations]
              [else
               (error "Unknown message: MACHINE" message)]))
      dispatch)))

(define (start machine) (machine 'start))
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name)
                 value)
  'register-set)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))













