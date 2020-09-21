#lang sicp

(define (tagged-list? exp tag)
  (and (pair? exp)
       (eq? (car exp) tag)))

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


; assembler
(define (assemble controller-text machine)
  (extract-labels
   controller-text
   (lambda (instructions labels)
     (update-insts! instructions labels machine)
     instructions)))

(define (extract-labels text on-receive)
  (if (null? text)
      (on-receive '() '())
      (extract-labels
       (cdr text)
       (lambda (instructions labels)
         (let ([next-instruction (car text)])
           (if (symbol? next-instruction)
               (if (assoc next-instruction labels)
                   (error
                    "unexpected label duplicate: EXTRACT-LABELS"
                    next-instruction)
                   (on-receive instructions
                               (cons (make-label-entry
                                      next-instruction
                                      instructions)
                                     labels)))
               (on-receive (cons (make-instruction
                                  next-instruction)
                                 instructions)
                           labels)))))))

(define (update-insts! instructions labels machine)
  (let ([pc (get-register machine 'pc)]
        [flag (get-register machine 'flag)]
        [stack (machine 'stack)]
        [operations (machine 'operations)])
    (for-each
     (lambda (instruction)
       (set-instruction-execution-proc!
        instruction
        (make-execution-procedure
         (instruction-text instruction)
         labels
         machine
         pc
         flag
         stack
         operations)))
     instructions)))

(define (make-instruction text) (cons text '()))
(define (instruction-text instruction) (car instruction))
(define (instruction-execution-proc instruction) (cdr instruction))
(define (set-instruction-execution-proc! instruction procedure)
  (set-cdr! instruction procedure))

(define (make-label-entry label-name instructions)
  (cons label-name instructions))

(define (lookup-label labels label-name)
  (let ([value (assoc label-name labels)])
    (if value
        (cdr value)
        (error "Undefined label: ASSEMBLE"
               label-name))))

; execution procedures
(define (make-execution-procedure instruction
                                  labels
                                  machine
                                  pc
                                  flag
                                  stack
                                  operations)
  (let ([type (car instruction)])
    (cond [(eq? type 'assign)
           (make-assign instruction machine labels operations pc)]
          [(eq? type 'test)
           (make-test instruction machine labels operations flag pc)]
          [(eq? type 'branch)
           (make-branch instruction machine labels flag pc)]
          [(eq? type 'goto)
           (make-goto instruction machine labels pc)]
          [(eq? type 'save)
           (make-save instruction machine stack pc)]
          [(eq? type 'restore)
           (make-restore instruction machine stack pc)]
          [(eq? type 'perform)
           (make-perform instruction machine labels operations pc)]
          [else
           (error "Uknown instruction type: ASSEMBLE"
                  instruction)])))

(define (make-assign instruction machine labels operations pc)
  (let ([target
         (get-register machine (assign-reg-name instruction))]
        [value-exp (assign-value-exp instruction)])
    (let ([value-procedure
           (if (operation-exp? value-exp)
               (make-operation-exp value-exp
                                   machine labels
                                   operations)
               (make-primitive-exp
                (car value-exp) machine labels))])
      (lambda () ; execution procedure for assign
        (set-contents! target (value-procedure))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (make-test instruction machine labels operations flag pc)
  (let ([condition (test-condition instruction)])
    (if (operation-exp? condition)
        (let ([condition-procedure
               (make-operation-exp
                condition machine labels operations)])
          (lambda ()
            (set-contents! flag (condition-procedure))
            (advance-pc pc)))
        (error "Invalid TEST instruction: ASSEMBLE"
               instruction))))

(define (test-condition test-instruction)
  (cdr test-instruction))

(define (make-branch instruction machine labels flag pc)
  (let ([destination (branch-dest instruction)])
    (if (label-exp? destination)
        (let ([instructions
               (lookup-label labels
                             (label-exp-label destination))])
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc instructions)
                (advance-pc pc))))
        (error "Invalid BRANCH instruction: ASSEMBLE"
               instruction))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto instruction machine labels pc)
  (let ([destination (goto-destination)])
    (cond [(label-exp? destination)
           (let ([instructions
                  (lookup-label
                   labels
                   (label-exp-label destination))])
             (lambda () (set-contents! pc instruction)))]
          [(register-exp? destination)
           (let ([register (get-register
                            machine
                            (register-exp-reg destination))])
             (lambda ()
               (set-contents! pc (get-contents register))))]
          [else
           (error "Invalid GOTO instruction: ASSEMBLE"
                  instruction)])))

(define (goto-destination goto-instruction)
  (cadr goto-instruction))

(define (make-save instruction machine stack pc)
  (let ([register
         (get-register
          machine
          (stack-inst-reg-name instruction))])
    (lambda ()
      (push stack (get-contents register))
      (advance-pc pc))))

(define (make-restore instruction machine stack pc)
  (let ([register
         (get-register
          machine
          (stack-inst-reg-name instruction))])
    (lambda ()
      (set-contents! register (pop stack))
      (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform instruction machine labels operations pc)
  (let ([action (perform-action instruction)])
    (if (operation-exp? action)
        (let ([action-procedure
               (make-operation-exp
                action machine labels operations)])
          (lambda ()
            (action-procedure)
            (advance-pc pc)))
        (error "Invalid PERFORM instruction: ASSEMBLE"
               instruction))))

(define (perform-action instruction) (cdr instruction))

(define (make-primitive-exp expression machine labels)
  (cond [(constant-exp? expression)
         (let ([value (constant-exp-value expression)])
           (lambda () value))]
        [(label-exp? expression)
         (let ([instructions
                (lookup-label
                 labels
                 (label-exp-label expression))])
           (lambda () instructions))]
        [(register-exp? expression)
         (let ([register
                (get-register machine
                              (register-exp-reg expression))])
           (lambda () (get-contents register)))]
        [else
         (error "Unknown expression type: ASSEMBLE"
                expression)]))

(define (register-exp? expression)
  (tagged-list? expression 'reg))
(define (register-exp-reg exp) (cadr exp))

(define (constant-exp? expression)
  (tagged-list? expression 'const))
(define (constant-exp-value exp) (cadr exp))

(define (label-exp? expression)
  (tagged-list? expression 'label))
(define (label-exp-label exp) (cadr exp))

(define (make-operation-exp expression machine labels operations)
  (let ([operation
         (lookup-operation (operation-exp-op expression)
                           operations)]
        [aprocs
         (map (lambda (exp)
                (if (label-exp? exp)
                    (error "Invalid operation expression: ASSEMBLE"
                           exp)
                    (make-primitive-exp exp machine labels)))
              (operation-exp-operands expression))])
    (lambda ()
      (apply operation
             (map (lambda (p) (p))
                  aprocs)))))

(define (operation-exp? expression)
  (and (pair? expression)
       (tagged-list? (car expression) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-operation symbol operations)
  (let ([value (assoc symbol operations)])
    (if value
        (cadr value)
        (error "Unknown operation: ASSEMBLE"
               symbol))))


















