#lang racket

(require sicp)

; a)
(meeting ?division (Friday . ?time))

; b)
(rule (meeting-time ?person ?day-and-time)
      (and (job ?person (?division . ?rest))
           (or (meeting ?division ?day-and-time)
               (meeting the-whole-company ?day-and-time))))

; c)
(meeting-time (Hacker Alyssa P) (Wednesday . ?time))

























