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
(require (only-in "table.rkt" put get))

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
        ;[(require? exp) (analyze-require exp)]
        [(if-fail? exp) (analyze-if-fail exp)]
        [(application? exp) (analyze-application exp)]
        [(eof-object? exp) (analyze-eof exp)]
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

(define (analyze-eof exp)
  (lambda (env succeed fail)
           (newline)
           (newline)
           (display ";;; No more problems :)")))

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
  (list (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list '< <)
        (list '> >)

        (list 'car car)
        (list 'cadr cadr)
        (list 'caddr caddr)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list 'pair? pair?)

        (list 'not not)
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list 'even? even?)
        (list 'assoc assoc)
        (list 'display display)
        (list 'newline newline)

        (list 'string->symbol string->symbol)
        (list 'string-append string-append)
        (list 'string=? string=?)
        (list 'substring substring)
        (list 'string-length string-length)

        (list 'number? number?)
        (list 'number->string number->string)

        (list 'symbol? symbol?)
        (list 'symbol->string symbol->string)

        (list 'tagged-list? tagged-list?)
        (list 'put put)
        (list 'get get)))

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

; ex 4.78
; require
(ambeval '(define (require p)
            (if (not p) (amb)))
         the-global-environment
         (lambda (success failure)
           (display "procedure installed: REQUIRE")
           (newline))
         (lambda ()
           (display "error while installing: REQUIRE")
           (newline)))

; an-element-of
(ambeval '(define (an-element-of items)
            (require (not (null? items)))
            (amb (car items) (an-element-of (cdr items))))
         the-global-environment
         (lambda (success failure)
           (display "procedure installed: AN-ELEMENT-OF")
           (newline))
         (lambda ()
           (display "error while installing: AN-ELEMENT-OF")
           (newline)))

; assertions
(ambeval '(begin
            (define assertions
              '((son Adam Cain)
                (son Cain Enoch)
                (son Enoch Irad)
                (son Irad Mehujael)
                (son Mehujael Methushael)
                (son Methushael Lamech)
                (wife Lamech Ada)
                (son Ada Jabal)
                (son Ada Jubal)))
            (define (add-assertion assertion frame)
              (set! assertions (cons assertion assertions)))
            (put 'assert! 'qeval add-assertion))
         the-global-environment
         (lambda (success failure)
           (display "ASSERTIONS successfully added")
           (newline))
         (lambda ()
           (display "failed to add ASSERTIONS")
           (newline)))

; utils
(ambeval '(begin
            (define (failed? exp)
              (eq? exp 'failed))
            (define (type exp)
              (if (pair? exp)
                  (car exp)
                  (error "Unknown expression: TYPE" exp)))
            (define (contents exp)
              (if (pair? exp)
                  (cdr exp)
                  (error "Unknown expression: CONTENTS" exp)))
            (define (expand-question-mark symbol)
              (let ([chars (symbol->string symbol)])
                (if (string=? (substring chars 0 1) "?")
                    (list '?
                          (string->symbol
                           (substring chars 1 (string-length chars))))
                    symbol)))
            (define (query-syntax-process exp)
              (map-over-symbols expand-question-mark exp))

            (define (map-over-symbols proc exp)
              (cond [(pair? exp)
                     (cons (map-over-symbols proc (car exp))
                           (map-over-symbols proc (cdr exp)))]
                    [(symbol? exp) (proc exp)]
                    [else exp]))
            (define (contract-question-mark variable)
              (string->symbol
               (string-append
                "?"
                (if (number? (cadr variable))
                    (string-append (symbol->string (caddr variable))
                                   "-"
                                   (number->string (cadr variable)))
                    (symbol->string (cadr variable))))))
            (define (variable? exp) (tagged-list? exp '?))
            (define (make-binding variable value)
              (cons variable value))
            (define (binding-variable binding) (car binding))
            (define (binding-value binding) (cdr binding))
            (define (binding-in-frame variable frame)
              (assoc variable frame))
            (define (extend variable value frame)
              (cons (make-binding variable value) frame)))
         the-global-environment
         (lambda (success failure)
           (display "procedure installed: UTILS")
           (newline))
         (lambda ()
           (display "error while installing: UTILS")
           (newline)))

; pattern-match
(ambeval '(begin
            (define (extend-if-consistent variable datum frame)
              (let ([binding (binding-in-frame variable frame)])
                (if binding
                    (pattern-match (binding-value binding)
                                   datum
                                   frame)
                    (extend variable datum frame))))
            (define (pattern-match pattern datum frame)
              (cond [(failed? frame) frame]
                    [(equal? pattern datum) frame]
                    [(variable? pattern)
                     (extend-if-consistent pattern datum frame)]
                    [(if (pair? pattern) (pair? datum))
                     (pattern-match
                      (cdr pattern)
                      (cdr datum)
                      (pattern-match (car pattern)
                                     (car datum)
                                     frame))]
                    [else 'failed])))
         the-global-environment
         (lambda (success failure)
           (display "procedure installed: PATTERN-MATCH")
           (newline))
         (lambda ()
           (display "error while installing: PATTERN-MATCH")
           (newline)))

; qeval
(ambeval '(begin
            (define (instantiate exp frame unbound-var-handler)
              (define (copy exp)
                (cond [(variable? exp)
                       (let ([binding (binding-in-frame exp frame)])
                         (if binding
                             (copy (binding-value binding))
                             (unbound-var-handler exp frame)))]
                      [(pair? exp)
                       (cons (copy (car exp))
                             (copy (cdr exp)))]
                      [else exp]))
              (copy exp))
            
            (define (qeval query frame)
              (let ([assertion (an-element-of assertions)])
                (let ([qproc (get (type query) 'qeval)])
                  (let ([match-result
                         (if qproc
                             (qproc (contents query) frame)
                             (pattern-match query assertion frame))])
                    ;(display assertion)
                    ;(newline)
                    ;(display match-result)
                    ;(newline)
                    ;(newline)
                    match-result))))

            (define (q query)
              (let ([processed-query
                     (query-syntax-process query)])
                (let ([frame
                       (qeval (query-syntax-process query)
                              '())])
                  (require (not (failed? frame)))
                  (instantiate
                    processed-query
                    frame
                    (lambda (variable _)
                      (contract-question-mark variable)))))))
         the-global-environment
         (lambda (success failure)
           (display "procedure installed: QEVAL")
           (newline))
         (lambda ()
           (display "error while installing: QEVAL")
           (newline)))

; conjoin
(ambeval '(begin
            (define (empty-conjunction? exps) (null? exps))
            (define (first-conjunct exps) (car exps))
            (define (rest-conjuncts exps) (cdr exps))
            (define (conjoin conjuncts frame)
              (cond [(empty-conjunction? conjuncts) frame]
                    [else (conjoin (rest-conjuncts conjuncts)
                                   (qeval (first-conjunct conjuncts)
                                          frame))]))
            (put 'and 'qeval conjoin))
         the-global-environment
         (lambda (success failure)
           (display "procedure installed: CONJOIN")
           (newline))
         (lambda ()
           (display "error while installing: CONJOIN")
           (newline)))

; disjoin
(ambeval '(begin
            (define (empty-disjunction? exps) (null? exps))
            (define (first-disjunct exps) (car exps))
            (define (rest-disjuncts exps) (cdr exps))
            (define (disjoin disjuncts frame)
              (if (empty-disjunction? disjuncts)
                  frame
                  (let ([result
                         (qeval (first-disjunct disjuncts)
                                frame)])
                    (if (failed? result)
                        (disjoin (rest-disjuncts disjuncts)
                                 frame)
                        result))))
            (put 'or 'qeval disjoin))
         the-global-environment
         (lambda (success failure)
           (display "procedure installed: DISJOIN")
           (newline))
         (lambda ()
           (display "error while installing: DISJOIN")
           (newline)))

; negate
(ambeval '(begin
            (define (negated-query exps) (car exps))
            (define (negate operands frame)
              (let ([result-frame
                     (qeval (negated-query operands)
                            frame)])
                ;(display result-frame)
                ;(newline)
                (if (failed? result-frame)
                    frame
                    'failed)))
            (put 'not 'qeval negate))
         the-global-environment
         (lambda (success failure)
           (display "procedure installed: NEGATE")
           (newline))
         (lambda ()
           (display "error while installing: NEGATE")
           (newline)))

(driver-loop)

; (q '(and (son ?x ?y) (not (son Adam ?x))))
; (q '(not (son . ?x)))

; There are still  gaps in solution.
; For example I didn't implement negate and rules.

; As we can see, all our fuss about stream processing,
; filtering and flattening is completely avoided because of backtracking mechanism

; The main difference consists in a fact that
; we receive all combinations of successful matches
; when we apply compound queries.













