#lang racket

; Turned out it's actually a difficult task especially
; if you use our evaluator to test solution,
; cause it lacks proper debugging and testing mechanism.

; I can still describe the way this excercise could be done here:

(define (apply-rule rule query-pattern query-frame)
  ; 1) We need to match pattern and conclusion of the rule in the frame to find binded vars
  ; pattern (grandson ?person Adam)
  ; rule    (grandson ?x ?y)
  ; => ((?x ?person) (?y Adam))
  ; note that this mechanism should first try to find ?person value
  ; and if there's no such value we use it as a binding
  (let ([conclusion-frame
         (match-conclusion (conclusion rule)
                           query-pattern
                           query-frame)])
    (if (failed? conclusion-frame)
        'failed
        ; 2) We split resulting frame into 2 frames.
        ; First frame should consist of conclusion variables bound to values
        ; rule-frame - ((?y Adam))
        ; Second will contain conclusion variables bound to pattern variables
        ; undbound-frame - ((?x ?person))
        (let ([rule-frame
               (variables-with-values conclusion-frame)]
              [unbound-frame
               (conclusion-pattern-variables conclusion-frame)])
          ; 3) We qeval rule body in first frame
          ; As result we will receive either
          ; frame with all variables of rule bound to values or 'failed frame.
          (let ([extended-rule-frame
                 (qeval (rule-body rule)
                        rule-frame)])
            (if (failed? extended-rule-frame)
                'failed
                ; 4) We bind variables in unbound-frame with same of the extended-rule-frame
                (let ([result-frame
                       (hydrate unbound-frame extended-rule-frame)])
                  ; 5) We return our query frame extended with hydrated-frame values)
                  (extend-frame query-frame result-frame))))))))















 
