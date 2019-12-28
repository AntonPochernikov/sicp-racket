#lang racket

(require sicp)

(rule (can-do-same-job ?person1 ?person2)
      (and (not (same person1? person2?))
           (job ?person1 ?job1)
           (job ?person2 ?job2)
           (or (same ?job1 ?job2)
               (can-do-job ?job1 ?job2))))

; a)
(can-do-same-job ?person (Fect Cy D.))

; b)
(and (can-do-same-job ?person1 ?person2)
     (salary ?person1 ?salary1)
     (salary ?person2 ?salary2)
     (lisp-value > ?salary1 ?salary2))
























