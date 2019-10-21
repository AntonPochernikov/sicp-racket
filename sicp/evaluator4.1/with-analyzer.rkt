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
(define (eval exp env) ((analyze exp) env))

(define (analyze exp)
  (cond [(self-evaluating? exp) (analyze-self-evaluating exp)]
        [(quoted? exp) (analyze-quoted exp)]
        [(variable? exp) (analyze-variable exp)]
        [(assignment? exp) (analyze-assignment exp)]
        [(definition? exp) (analyze-definition exp)]
        [(if? exp) (analyze-if exp)]
        [(lambda? exp) (analyze-lambda exp)]
        [(begin? exp) (analyze-sequence (begin-actions exp))]
        [(cond? exp) (analyze (cond->if exp))]
        [(let? exp) (analyze (let->combination exp))]
        [(application? exp) (analyze-application exp)]
        [else (error "Unknown expression type: ANALYZE" exp)]))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ([text (text-of-quotation exp)])
    (lambda (env) text)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ([var (assignment-variable exp)]
        [vproc (analyze (assignment-value exp))])
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ([var (definition-variable exp)]
        [vproc (analyze (definition-value exp))])
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  (let ([pproc (analyze (if-predicate exp))]
        [cproc (analyze (if-consequent exp))]
        [aproc (analyze (if-alternative exp))])
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))

(define (analyze-lambda exp)
  (let ([vars (lambda-parameters exp)]
        [bproc (analyze-sequence (lambda-body exp))])
    (lambda (env)
      (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentally proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentally first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ([procs (map analyze exps)])
    (if (null? procs)
        (error "Empty sequence: ANALYZE")
        (loop (car procs) (cdr procs)))))

(define (analyze-application exp)
  (let ([fproc (analyze (operator exp))]
        [aprocs (map analyze (operands exp))])
    (lambda (env)
      (execute-application
       (fproc env)
       (map (lambda (aproc) (aproc env))
            aprocs)))))

(define (execute-application proc args)
  (cond [(primitive-procedure? proc)
         (apply-primitive-procedure proc args)]
        [(compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment
           (procedure-parameters proc)
           args
           (procedure-environment proc)))]
        [else
         (error
          "Unknown procedure type: EXECUTE-APPLICATION"
          proc)]))

; ==============================================
; SPECIAL FORMS
; ==============================================
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

; LET*
(define (eval-let* exp env)
  (eval (let*->nested-lets exp) env))

; APPLICATION
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

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
        (list '< <)))

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
          [output (eval input the-global-environment)]
          [end-time (current-inexact-milliseconds)])
      (display "Execution time:")
      (display (- end-time start-time))
      (display "ms")
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(driver-loop)
















