#lang sicp

; I will slightly rearrange syntax to make it similar
; to other programming languages
; Let's replace 'test' operation with 'if'
; and 'branch' with 'then'.
; It'll be also more convinient to rename 'op'
; to 'apply' and make it look like a procedure.

; Hence the syntax is the following:
; test-b
; (if (apply = (reg b) (const 0)))
; (then (label gcd-done))
; (assign t (apply rem (reg a) (reg b)))
; (assign a (reg b))
; (assign b (reg t))
; (goto (label test-b))
; gcd-done

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
          ; rename operations
          [(eq? type 'if)
           (make-if instruction machine labels operations flag pc)]
          [(eq? type 'then)
           (make-then instruction machine labels flag pc)]
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

; IF
(define (make-if instruction machine labels operations flag pc)
  (let ([condition (if-condition instruction)])
    (if (operation-exp? condition)
        (let ([condition-procedure
               (make-operation-exp
                condition machine labels operations)])
          (lambda ()
            (set-contents! flag (condition-procedure))
            (advance-pc pc)))
        (error "Invalid IF instruction: ASSEMBLE"
               instruction))))

(define (if-condition if-instruction)
  (cdr if-instruction))

; THEN
(define (make-then instruction machine labels flag pc)
  (let ([destination (then-dest instruction)])
    (if (label-exp? destination)
        (let ([instructions
               (lookup-label labels
                             (label-exp-label destination))])
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc instructions)
                (advance-pc pc))))
        (error "Invalid THEN instruction: ASSEMBLE"
               instruction))))

(define (then-dest then-instruction)
  (cadr then-instruction))

; APPLY
(define (operation-exp? expression)
       (tagged-list? expression 'apply))

; changing selectors is enough to setup new syntax
(define (operation-exp-op operation-exp)
  (cadr operation-exp))

(define (operation-exp-operands operation-exp)
  (cddr operation-exp))
















