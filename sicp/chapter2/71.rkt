#lang sicp

; n = 5
;
;      (1 2 4 8 16)
; (1 2 4 8) /\
;  (1 2 4) /\ 16
;   (1 2) /\ 8
;        /\ 4
;       1  2
;
; sum of 2 "lightest" symbols are 1 less then next symbol on every iteration
; so we will get same type of tree for 5 and 10 symbols
; 1 bit for most common symbol
; n - 1 bits for most rare symbol
