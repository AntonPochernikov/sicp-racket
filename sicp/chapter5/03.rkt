#lang racket

(require sicp)

; with good-enough? and improve as primitives
(controller
 sqrt-loop
   (assing x (op read))
   (assign g (const 1.0))
 test-g
   (test (op good-enough?)
         (reg g)
         (reg x))
   (branch (label sqrt-done))
   (assign t (op improve) (reg g) (reg x))
   (assign g (reg t))
   (goto (label test-g))
 sqrt-done
   (perform (op print)
            (reg g))
   (goto (label sqrt-loop)))

; in terms of arithmetic operations

(controller
 sqrt-loop
   (assing x (op read))
   (assign guess (const 1.0))

 test-guess
   (assign square-g (op *) (reg guess) (reg guess))
   (assign delta (op -) (reg square-g) (reg x))

 abs-start
   (test (op >) (reg delta) (const 0))
   (branch (label abs-positive))
   (assign abs (op -) (reg delta))
   (goto (label abs-end))
   
 abs-positive
   (assign abs (reg delta))

 abs-end

   (test (op <)
         (reg abs)
         (const 0.001))
   (branch (label sqrt-done))
 
   (assign x/g (op /) (reg x) (reg guess))
   (assign sum (op +) (reg x/g) (reg guess))
   (assign guess (op /) (reg sum) (const 2))
   (goto (label test-guess))

 sqrt-done
   (perform (op print)
            (reg guess))
   (goto (label sqrt-loop)))

















