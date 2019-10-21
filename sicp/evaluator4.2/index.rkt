#lang racket

(require sicp)
(require (only-in "eval-apply.rkt" apply-in-underlying-scheme))
(require (only-in "./utils/tagged-list.rkt" tagged-list?))
(require (only-in "./expressions/self-evaluating.rkt" self-evaluating?))
(require (only-in "./expressions/variable.rkt" variable?))
(require (only-in "./expressions/quote.rkt" quoted? text-of-quotation))
(require (only-in "./expressions/if.rkt"
                  make-if
                  if?
                  if-predicate
                  if-consequent
                  if-alternative))
(require (only-in "./expressions/begin.rkt"
                  make-begin
                  begin?
                  begin-actions
                  sequence->exp
                  last-exp?
                  first-exp
                  rest-exps))
(require (only-in "./expressions/cond.rkt" cond? cond->if))
(require (only-in "./expressions/assignment.rkt"
                  assignment?
                  assignment-variable
                  assignment-value))
(require (only-in "./expressions/lambda.rkt"
                  lambda?
                  make-lambda
                  lambda-parameters
                  lambda-body))
(require (only-in "./expressions/definition.rkt"
                  definition?
                  definition-variable
                  definition-value))
(require (only-in "./expressions/let.rkt"
                  make-let
                  let?
                  let->combination
                  let*?
                  let*->nested-lets))
(require (only-in "environment.rkt"
                  the-empty-environment
                  extend-environment
                  lookup-variable-value
                  set-variable-value!
                  define-variable!))

(provide eval the-global-environment)

; ==============================================
; EVAL
; ==============================================
(define (eval exp env)
  (cond [(self-evaluating? exp) exp]
        [(variable? exp) (lookup-variable-value exp env)]
        [(quoted? exp) (text-of-quotation exp)]
        [(assignment? exp) (eval-assignment exp env)]
        [(definition? exp) (eval-definition exp env)]
        [(if? exp) (eval-if exp env)]
        [(lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env)]
        [(begin? exp)
         (eval-sequence (begin-actions exp) env)]
        [(cond? exp) (eval (cond->if exp) env)]
        [(let? exp) (eval-let exp env)]
        [(let*? exp) (eval-let* exp env)]
        [(unless? exp) (eval (unless->if exp) env)]
        [(application? exp)
         (apply (actual-value (operator exp) env)
                (operands exp)
                env)]
        [else
         (error "Unknown expression type: EVAL" exp)]))

; ==============================================
; APPLY
; ==============================================
(define (apply procedure arguments env)
  (cond [(primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env))]
        [(compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env)
           (procedure-environment procedure)))]
        [else
         (error "Unknown procedure type: APPLY" procedure)]))

; ==============================================
; SPECIAL FORMS
; ==============================================
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

; IF
(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

; BEGIN
(define (eval-sequence exps env)
  (cond [(last-exp? exps)
         (eval (first-exp exps) env)]
        [else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env)]))

; ASSIGNMENT
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

; DEFINITION
(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (eval (definition-value exp)
          env)
    env)
  'ok)

; LET
(define (eval-let exp env)
  (eval (let->combination exp) env))

; LET*
(define (eval-let* exp env)
  (eval (let*->nested-lets exp) env))

(define (unless? exp) (tagged-list? exp 'unless))
(define (unless->if exp)
  (list 'if (cadr exp) (cadddr exp) (caddr exp)))

; APPLICATION
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (actual-value exp env)
  (force-it (eval exp env)))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons
       (actual-value (first-operand exps)
                     env)
       (list-of-arg-values (rest-operands exps)
                           env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps)
                      env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))

; THUNK
(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk)
  (cadr thunk))
(define (thunk-env thunk)
  (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk)
  (cadr evaluated-thunk))

(define (force-it obj)
  (cond [(thunk? obj)
         (let ([result
                (actual-value (thunk-exp obj)
                              (thunk-env obj))])
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)
           (set-cdr! (cdr obj) '())
           result)]
        [(evaluated-thunk? obj) (thunk-value obj)]
        [else obj]))

(define (delay-it exp env)
  (list 'thunk exp env))

; PROCEDURE
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p) (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc)
   args))

; ==============================================
; SETUP INITIAL ENVIRONMENT
; ==============================================
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list '< <)
        (list 'set! set)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ([initial-env
         (extend-environment
          (primitive-procedure-names)
          (primitive-procedure-objects)
          the-empty-environment)])
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

; ==============================================
; RUN PROMPT
; ==============================================
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (prompt-for-input string)
  (newline)
  (display string)
  (newline))

(define (announce-output string)
  (newline)
  (display string)
  (newline)
  (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ([input (read)])
    (let ([start-time (current-inexact-milliseconds)]
          [output (actual-value input the-global-environment)]
          [end-time (current-inexact-milliseconds)])
      (display "Execution time:")
      (display (- end-time start-time))
      (display "ms")
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(driver-loop)







         








