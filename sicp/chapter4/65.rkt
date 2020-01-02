#lang racket

(require sicp)

(rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
           (supervisor ?x ?middle-manager)))

; (wheel who?)

; f0 []

; f1 [?person: ?who]
; evaluating (supervisor ?middle-manager ?who) against f1

; f2 has 8 frames:
; [?middle-manager: (Hacker Alyssa P), ?who: (Bitdiddle Ben)]
; [?middle-manager: (Fect Cy D), ?who: (Bitdiddle Ben)]
; [?middle-manager: (Tweakit Lem E), ?who: (Bitdiddle Ben)]
; [?middle-manager: (Reasoner Louis), ?who: (Hacker Alyssa P)]
; [?middle-manager: (Bitdiddle Ben), ?who: (Warbucks Oliver)]
; [?middle-manager: (Scrooge Eben), ?who: (Warbucks Oliver)]
; [?middle-manager: (Cratchet Robert), ?who: (Scrooge Eben)]
; [?middle-manager: (Aull DeWitt), ?who: (Warbucks Oliver)]

; f3 has 5 frames:
; [?x: (Hacker Alyssa P), ?middle-manager: (Bitdiddle Ben), ?who: (Warbucks Oliver)]
; [?x: (Fect Cy D), ?middle-manager: (Bitdiddle Ben), ?who: (Warbucks Oliver)]
; [?x: (Tweakit Lem E), ?middle-manager: (Bitdiddle Ben), ?who: (Warbucks Oliver)]
; [?x: (Reasoner Louis), ?middle-manager: (Hacker Alyssa P), ?who: (Bitdiddle Ben)]
; [?x: (Cratchet Robert), ?middle-manager: (Scrooge Eben), ?who: (Warbucks Oliver)]

; These are results of who that would be printed:
; (wheel (Warbucks Oliver))
; (wheel (Warbucks Oliver))
; (wheel (Warbucks Oliver))
; (wheel (Bitdiddle Ben))
; (wheel (Warbucks Oliver))











