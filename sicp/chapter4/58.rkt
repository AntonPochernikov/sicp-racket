#lang racket

(require sicp)

(rule (bigshot ?person ?division)
      (and (job ?person (?division . ?r1))
           (or (not (supervisor ?person ?boss))
               (and (supervisor ?person ?boss)
                    (not (job ?boss (?division . ?r2)))
                    (not (bigshot ?boss ?division))))))





















