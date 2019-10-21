#lang racket

(require sicp)

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
        ; returns one procedure that contains whole sequence analyzed
        (loop (car procs) (cdr procs)))))

(define (analyze-sequence-2 exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs))
           ((car procs) env))
          (else ((car procs) env)
                (execute-sequence (cdr procs)
                                  env))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence: ANALYZE"))
    ; returns procedure that will generate new analyzed sequence every time
    (lambda (env)
      (execute-sequence procs env))))

; 2nd example will call execute-sequence on every eval of this sequence
; that way we will recompute sequence





