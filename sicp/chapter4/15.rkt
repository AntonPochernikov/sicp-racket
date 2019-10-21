#lang racket

; If given expression (halt? try try) will return true, (meaning that it halts)
; our 'try' procedure will run forever, so it actually can't halt.

; If (halt? try try) will return false (meaning that it won't halt)
; the return will be 'halt.
