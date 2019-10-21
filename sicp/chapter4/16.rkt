#lang racket

(require sicp)

; SOLUTION
; a
(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ([ref (assoc var (frame-pairs (first-frame env)))])
          (if ref
              (if (eq? (cdr ref) '*unassigned*)
                  (error
                   "Attemting to use value of unassigned variable"
                   var)
                  (cdr ref))
              (env-loop (enclosing-environment env))))))
  (env-loop env))

; b
(define (scan-out-defines body)
  (let ([definitions (filter definition? body)]
        [rest-body (filter (lambda (exp)
                        (not (definition? exp)))
                      body)])
    (if (null? definitions)
        body
        (let ([vars (map definition-variable definitions)]
              [vals (map definition-value definitions)])
          (make-let (map (lambda (var)
                           (list var '*unassigned*))
                         vars)
                    (append
                     (map (lambda (var val)
                            (list 'set! var val))
                          vars
                          vals)
                     rest-body))))))

; c
; Installing scan-out-defines in make-procedure is better
; in terms of performance.
; Thus we will compute proc body at definition.
; Installing in procedure-body will cause recomputing
; compound procedure body on every call.
(define (make-procedure parameters body env)
  (list 'procedure
        parameters
        (scan-out-defines body)
        env))








