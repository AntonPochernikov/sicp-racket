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
(require (only-in "./expressions/if-fail.rkt"
                  if-fail?
                  if-fail-consequent
                  if-fail-alternative))

(provide ambeval the-global-environment)

; ==============================================
; AMB-EVAL
; ==============================================
(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze exp)
  (cond [(self-evaluating? exp) (analyze-self-evaluating exp)]
        [(quoted? exp) (analyze-quoted exp)]
        [(variable? exp) (analyze-variable exp)]
        [(assignment? exp) (analyze-assignment exp)]
        [(permanent-assignment? exp) (analyze-permanent-assignment exp)]
        [(definition? exp) (analyze-definition exp)]
        [(if? exp) (analyze-if exp)]
        [(lambda? exp) (analyze-lambda exp)]
        [(begin? exp) (analyze-sequence (begin-actions exp))]
        [(cond? exp) (analyze (cond->if exp))]
        [(let? exp) (analyze (let->combination exp))]
        [(let*? exp) (analyze (let*->nested-lets exp))]
        [(amb? exp) (analyze-amb exp)]
        [(ramb? exp) (analyze-ramb exp)]
        [(require? exp) (analyze-require exp)]
        [(if-fail? exp) (analyze-if-fail exp)]
        [(application? exp) (analyze-application exp)]
        [else (error "Unknown expression type: ANALYZE" exp)]))

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ([text (text-of-quotation exp)])
    (lambda (env succeed fail)
      (succeed text fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env) fail)))

(define (analyze-assignment exp)
  (let ([variable (assignment-variable exp)]
        [vproc (analyze (assignment-value exp))])
    (lambda (env succeed fail)
      (vproc env
             (lambda (value fail2)
               (let ([old-value
                      (lookup-variable-value variable env)])
                 (set-variable-value! variable
                                      value
                                      env)
                 (succeed 'ok
                          (lambda ()
                            (set-variable-value!
                             variable
                             old-value
                             env)
                            (fail2)))))
             fail))))

(define (analyze-permanent-assignment exp)
  (let ([variable (assignment-variable exp)]
        [vproc (analyze (assignment-value exp))])
    (lambda (env succeed fail)
      (vproc env
             (lambda (value fail2)
               (set-variable-value! variable
                                    value
                                    env)
               (succeed 'ok
                        fail2))
             fail))))

(define (analyze-definition exp)
  (let ([var (definition-variable exp)]
        [vproc (analyze (definition-value exp))])
    (lambda (env succeed fail)
      (vproc env
             (lambda (value fail2)
               (define-variable! var value env)
               (succeed 'ok fail2))
             fail))))

(define (analyze-if exp)
  (let ([pproc (analyze (if-predicate exp))]
        [cproc (analyze (if-consequent exp))]
        [aproc (analyze (if-alternative exp))])
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value pred-fail)
               (if (true? pred-value)
                   (cproc env succeed pred-fail)
                   (aproc env succeed pred-fail)))
             fail))))

(define (analyze-lambda exp)
  (let ([vars (lambda-parameters exp)]
        [bproc (analyze-sequence (lambda-body exp))])
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env) fail))))

(define (analyze-sequence exps)
  (define (sequentally a b)
    (lambda (env succeed fail)
      (a env
         (lambda (a-value a-fail)
           (b env succeed a-fail))
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentally first-proc
                           (car rest-procs))
              (cdr rest-procs))))
  (let ([procs (map analyze exps)])
    (if (null? procs)
        (error "Empty sequence: ANALYZE")
        (loop (car procs) (cdr procs)))))

(define (analyze-amb exp)
  (let ([cprocs (map analyze (amb-choices exp))])
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices)
             env
             succeed
             (lambda ()
               (try-next (cdr choices))))))
      (try-next cprocs))))

(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (not pred-value)
                   (fail2)
                   (succeed 'ok fail2)))
             fail))))

; analyze-ramb utils
(define (length L)
  (if (null? L)
      0
      (+ 1 (length (cdr L)))))

(define (ref n L)
  (cond [(null? L) false]
        [(= n 0) (car L)]
        [else
         (ref (- n 1)
              (cdr L))]))

(define (sample L)
  (let ([index (random (length L))])
    (ref index L)))

(define (filter pred? L)
  (cond [(null? L) '()]
        [(pred? (car L))
         (cons (car L)
               (filter pred? (cdr L)))]
        [else
         (filter pred? (cdr L))]))

(define (without item L)
  (filter (lambda (elem)
            (not (eq? elem item)))
          L))

(define (analyze-ramb exp)
  (let ([cprocs (map analyze (ramb-choices exp))])
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            (let ([current-choice (sample choices)])
              (current-choice
               env
               succeed
               (lambda ()
                 (try-next (without current-choice choices)))))))
      (try-next cprocs))))

(define (analyze-if-fail exp)
  (let ([consequent (analyze (if-fail-consequent exp))]
        [alternative (analyze (if-fail-alternative exp))])
    (lambda (env succeed fail)
      (consequent env
                  succeed
                  (lambda ()
                    (alternative env
                                 succeed
                                 fail))))))

; analyze-application util
(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs)
       env
       (lambda (arg fail-arg)
         (get-args
          (cdr aprocs)
          env
          (lambda (args fail-args)
            (succeed (cons arg args) fail-args))
          fail-arg))
       fail)))

(define (analyze-application exp)
  (let ([fproc (analyze (operator exp))]
        [aprocs (map analyze (operands exp))])
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail-proc)
               (get-args aprocs
                         env
                         (lambda (args fail-args)
                           (execute-application
                            proc
                            args
                            succeed
                            fail-args))
                         fail-proc))
             fail))))

(define (execute-application proc args succeed fail)
  (cond [(primitive-procedure? proc)
         (succeed
          (apply-primitive-procedure proc args)
          fail)]
        [(compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment
           (procedure-parameters proc)
           args
           (procedure-environment proc))
          succeed
          fail)]
        [else
         (error "Unknown procedure type: EXECUTE-APPLICATION"
                proc)]))

; ==============================================
; SPECIAL FORMS
; ==============================================
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

; AMB
(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

; RAMB
(define (ramb? exp) (tagged-list? exp 'ramb))
(define (ramb-choices exp) (cdr exp))

; REQUIRE
(define (require? exp)
  (tagged-list? exp 'require))
(define (require-predicate exp)
  (cadr exp))

; PERMANENT-ASSIGNMENT
(define (permanent-assignment? exp)
  (tagged-list? exp 'permanent-set!))

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
        (list 'not not)
        (list 'eq? eq?)
        (list 'even? even?)
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
(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

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
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ([input (read)])
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ";;; Starting a new problem")
            (ambeval
             input
             the-global-environment
             (lambda (value next-alternative)
               (announce-output output-prompt)
               (user-print value)
               (internal-loop next-alternative))
             (lambda ()
               (announce-output ";; There are no more values of")
               (user-print input)
               (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop))))

(driver-loop)
















