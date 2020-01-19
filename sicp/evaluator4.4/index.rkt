#lang racket

(require sicp)
(require (only-in "table.rkt" put get))

; DRIVER-LOOP
(define input-prompt "--- Query input:")
(define output-prompt "--- Query results:")

(define (prompt-for-input string)
  (newline)
  (display string)
  (newline))

(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ([q (query-syntax-process (read))])
    (cond [(assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base.")
           (newline)
           ; try to move it out of the cond clauses
           (query-driver-loop)]
          [else
           (display output-prompt)
           (display-stream
            (stream-map
             (lambda (frame)
               (instantiate
                 q
                 frame
                 (lambda (variable _)
                   (contract-question-mark variable))))
             (qeval q (singleton-stream '()))))
           (query-driver-loop)])))

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

(define user-initial-environment '())

; QEVAL
(define (qeval query frame-stream)
  (let ([qproc (get (type query) 'qeval)])
    (if qproc
        (qproc (contents query) frame-stream)
        (simple-query query frame-stream))))

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed
      (find-assertions query-pattern frame)
      (delay (apply-rules query-pattern frame))))
   frame-stream))

; conjoin
(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts)
                      frame-stream))))

(put 'and 'qeval conjoin)

; disjoin
(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts)
                       frame-stream)))))

(put 'or 'qeval disjoin)

; negate
(define (negated-query exps) (car exps))

(define (negate operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (stream-null?
          (qeval (negated-query operands)
                 (singleton-stream frame)))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(put 'not 'qeval negate)

; lisp-value
(define (lisp-value call frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (execute
          (instantiate
            call
            frame
            (lambda (v f)
              (error "Unknown pattern variable: LISP-VALUE" v))))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(put 'lisp-value 'qeval lisp-value)

(define (predicate exps) (car exps))
(define (args exps) (cdr exps))

(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment)
         (args exp)))

; always-true
(define (always-true _ frame-stream) frame-stream)
(put 'always-true 'qeval always-true)

; unique
(define (unique-query exp) (car exp))

(define (uniquely-asserted contents frame-stream)
  (stream-flatmap
   (lambda (frame)
     (let ([result-stream
            (qeval (unique-query contents)
                   (singleton-stream frame))])
       (if (singleton-stream? result-stream)
           result-stream
           the-empty-stream)))
   frame-stream))

(put 'unique 'qeval uniquely-asserted)

; ASSERTIONS
(define (find-assertions pattern frame)
  (stream-flatmap
   (lambda (datum)
     (check-an-assertion datum pattern frame))
   (fetch-assertions pattern frame)))

(define (check-an-assertion assertion
                            query-pattern
                            query-frame)
  (let ([match-result
         (pattern-match query-pattern
                        assertion
                        query-frame)])
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream match-result))))

(define (pattern-match pattern datum frame)
  (cond [(eq? frame 'failed) 'failed]
        [(equal? pattern datum) frame]
        [(variable? pattern)
         (extend-if-consistent pattern datum frame)]
        [(and (pair? pattern) (pair? datum))
         (pattern-match
          (cdr pattern)
          (cdr datum)
          (pattern-match (car pattern)
                         (car datum)
                         frame))]
        [else 'failed]))

(define (extend-if-consistent variable datum frame)
  (let ([binding (binding-in-frame variable frame)])
    (if binding
        (pattern-match (binding-value binding)
                       datum
                       frame)
        (extend variable datum frame))))

; RULES AND UNIFICATION
(define (apply-rules pattern frame)
  (stream-flatmap
   (lambda (rule)
     (apply-a-rule rule pattern frame))
   (fetch-rules pattern frame)))

(define (apply-a-rule rule query-pattern query-frame)
  (let ([clean-rule (rename-variables-in rule)])
    (let ([unified-result
           (unify-match query-pattern
                        (conclusion clean-rule)
                        query-frame)])
      (if (eq? unified-result 'failed)
          the-empty-stream
          (qeval (rule-body clean-rule)
                 (singleton-stream unified-result))))))

(define (rename-variables-in rule)
  (let ([rule-application-id (new-rule-application-id)])
    (define (tree-walk exp)
      (cond [(variable? exp)
             (make-new-variable exp rule-application-id)]
            [(pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp)))]
            [else exp]))
    (tree-walk rule)))

(define (unify-match p1 p2 frame)
  (cond [(eq? frame 'failed) 'failed]
        [(equal? p1 p2) frame]
        [(variable? p1)
         (extend-if-possible p1 p2 frame)]
        [(variable? p2)
         (extend-if-possible p2 p1 frame)]
        [(and (pair? p1) (pair? p2))
         (unify-match (cdr p1)
                      (cdr p2)
                      (unify-match (car p1)
                                   (car p2)
                                   frame))]
        [else 'failed]))

(define (extend-if-possible variable value frame)
  (let ([binding (binding-in-frame variable frame)])
    (cond [binding
           (unify-match (binding-value binding)
                        value
                        frame)]
          [(variable? value)
           (let ([binding (binding-in-frame value frame)])
             (if binding
                 (unify-match variable
                              (binding-value binding)
                              frame)
                 (extend variable value frame)))]
          [(depends-on? value variable frame)
           'failed]
          [else (extend variable value frame)])))

(define (depends-on? exp variable frame)
  (define (tree-walk exp)
    (cond [(variable? exp)
           (if (equal? variable exp)
               true
               (let ([binding (binding-in-frame exp frame)])
                 (if binding
                     (tree-walk (binding-value binding))
                     false)))]
          [(pair? exp)
           (or (tree-walk (car exp))
               (tree-walk (cdr exp)))]
          [else false]))
  (tree-walk exp))

; ASSERTIONS
(define THE-ASSERTIONS the-empty-stream)

(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))

(define (get-all-assertions) THE-ASSERTIONS)

(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))

(define (get-stream key1 key2)
  (let ([s (get key1 key2)])
    (if s s the-empty-stream)))

(define THE-RULES the-empty-stream)

(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))

(define (get-all-rules) THE-RULES)

(define (get-indexed-rules pattern)
  (stream-append
   (get-stream (index-key-of pattern) 'rule-stream)
   (get-stream '? 'rule-stream)))

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ([old-assertions THE-ASSERTIONS])
    (set! THE-ASSERTIONS
          (cons-stream assertion old-assertions))
    'ok))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ([old-rules THE-RULES])
    (set! THE-RULES (cons-stream rule old-rules))
    'ok))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ([key (index-key-of assertion)])
        (let ([current-assertion-stream
               (get-stream key 'assertion-stream)])
          (put key
               'assertion-stream
               (cons-stream
                assertion
                current-assertion-stream))))))

(define (store-rule-in-index rule)
  (let ([pattern (conclusion rule)])
    (if (indexable? pattern)
        (let ([key (index-key-of pattern)])
          (display key)
          (let ([current-rule-stream
                 (get-stream key 'rule-stream)])
            (put key
                 'rule-stream
                 (cons-stream rule
                              current-rule-stream)))))))

(define (indexable? pattern)
  (or (constant-symbol? (car pattern))
      (variable? (car pattern))))

(define (index-key-of pattern)
  (let ([key (car pattern)])
    (if (variable? key) '? key)))

(define (use-index? pattern)
  (constant-symbol? (car pattern)))

; STREAM OPERATIONS
(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))

(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream
       (stream-car s1)
       (stream-append (stream-cdr s1) s2))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream
       (proc (stream-car s))
       (stream-map proc (stream-cdr s)))))

(define (display-stream s)
  (if (null? s)
      'done
      (begin
        (newline)
        (display (stream-car s))
        (display-stream (stream-cdr s)))))

(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (stream-append-delayed
        (stream-cdr s1)
        delayed-s2))))

(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (interleave-delayed
        (force delayed-s2)
        (delay (stream-cdr s1))))))

(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))

(define (flatten-stream s)
  (if (stream-null? s)
      the-empty-stream
      (interleave-delayed
       (stream-car s)
       (delay (flatten-stream (stream-cdr s))))))

(define (singleton-stream x)
  (cons-stream x the-empty-stream))

(define (singleton-stream? s)
  (and (not (stream-null? s))
       (stream-null? (stream-cdr s))))

; FRAMES AND BINDINGS
(define (make-binding variable value)
  (cons variable value))
(define (binding-variable binding) (car binding))
(define (binding-value binding) (cdr binding))
(define (binding-in-frame variable frame)
  (assoc variable frame))
(define (extend variable value frame)
  (cons (make-binding variable value) frame))

; QUERY SYNTAX PROCEDURES
(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression: TYPE" exp)))

(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression: CONTENTS" exp)))

(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))

(define (add-assertion-body exp)
  (car (contents exp)))

(define (rule? statement)
  (tagged-list? statement 'rule))

(define (conclusion rule) (cadr rule))

(define (rule-body rule)
  (if (null? (cddr rule))
      '(always-true)
      (caddr rule)))

(define (tagged-list? exp tag)
  (and (pair? exp)
       (eq? (car exp) tag)))

(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))

(define (map-over-symbols proc exp)
  (cond [(pair? exp)
         (cons (map-over-symbols proc (car exp))
               (map-over-symbols proc (cdr exp)))]
        [(symbol? exp) (proc exp)]
        [else exp]))

(define (expand-question-mark symbol)
  (let ([chars (symbol->string symbol)])
    (if (string=? (substring chars 0 1) "?")
        (list '?
              (string->symbol
               (substring chars 1 (string-length chars))))
        symbol)))

(define (variable? exp) (tagged-list? exp '?))
(define (constant-symbol? exp) (symbol? exp))

(define rule-counter 0)
(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)

(define (make-new-variable variable rule-application-id)
  (cons '? (cons rule-application-id (cdr variable))))

(define (contract-question-mark variable)
  (string->symbol
   (string-append
    "?"
    (if (number? (cadr variable))
        (string-append (symbol->string (caddr variable))
                       "-"
                       (number->string (cadr variable)))
        (symbol->string (cadr variable))))))

(add-assertion! '(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10)))
(add-assertion! '(job (Bitdiddle Ben) (computer wizard)))
(add-assertion! '(salary (Bitdiddle Ben) 60000))

(add-assertion! '(address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
(add-assertion! '(job (Hacker Alyssa P) (computer programmer)))
(add-assertion! '(salary (Hacker Alyssa P) 40000))
(add-assertion! '(supervisor (Hacker Alyssa P) (Bitdiddle Ben)))

(add-assertion! '(address (Fect Cy D) (Cambridge (Ames Street) 3)))
(add-assertion! '(job (Fect Cy D) (computer programmer)))
(add-assertion! '(salary (Fect Cy D) 35000))
(add-assertion! '(supervisor (Fect Cy D) (Bitdiddle Ben)))

(add-assertion! '(address (Tweakit Lem E) (Boston (Bay State Road) 22)))
(add-assertion! '(job (Tweakit Lem E) (computer technician)))
(add-assertion! '(salary (Tweakit Lem E) 25000))
(add-assertion! '(supervisor (Tweakit Lem E) (Bitdiddle Ben)))

(add-assertion! '(address (Reasoner Louis) (Slumerville (Pine Tree Road) 80)))
(add-assertion! '(job (Reasoner Louis) (computer programmer trainee)))
(add-assertion! '(salary (Reasoner Louis) 30000))
(add-assertion! '(supervisor (Reasoner Louis) (Hacker Alyssa P)))

(add-assertion! '(supervisor (Bitdiddle Ben) (Warbucks Oliver)))
(add-assertion! '(address (Warbucks Oliver) (Swellesley (Top Heap Road))))
(add-assertion! '(job (Warbucks Oliver) (administration big wheel)))
(add-assertion! '(salary (Warbucks Oliver) 150000))

(add-assertion! '(address (Scrooge Eben) (Weston (Shady Lane) 10)))
(add-assertion! '(job (Scrooge Eben) (accounting chief accountant)))
(add-assertion! '(salary (Scrooge Eben) 75000))
(add-assertion! '(supervisor (Scrooge Eben) (Warbucks Oliver)))

(add-assertion! '(address (Cratchet Robert) (Allston (N Harvard Street) 16)))
(add-assertion! '(job (Cratchet Robert) (accounting scrivener)))
(add-assertion! '(salary (Cratchet Robert) 18000))
(add-assertion! '(supervisor (Cratchet Robert) (Scrooge Eben)))

(add-assertion! '(address (Aull DeWitt) (Slumerville (Onion Square) 5)))
(add-assertion! '(job (Aull DeWitt) (administration secretary)))
(add-assertion! '(salary (Aull DeWitt) 25000))
(add-assertion! '(supervisor (Aull DeWitt) (Warbucks Oliver)))

(add-assertion! '(can-do-job (computer wizard) (computer programmer)))
(add-assertion! '(can-do-job (computer wizard) (computer technician)))
(add-assertion!
 '(can-do-job (computer programmer)
              (computer programmer trainee)))
(add-assertion!
 '(can-do-job (administration secretary)
              (administration big wheel)))

(query-driver-loop)











