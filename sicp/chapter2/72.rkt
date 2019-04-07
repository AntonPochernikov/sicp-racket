#lang sicp

; its hard to count exact number of step
; usually number of steps depends on tree structure

; if we are talking about exercise 2.71 the most frequent symbol in the tree will be just O(1)
; but we have to check if its in branch symbols, so it depends on tree depth and will be O(n) in our case

; we need to process n - 1 nodes in order to reach last symbol, so its O(n(n - 1)) = O(n ** 2)
