#lang racket

(require sicp)

; specification of factorial machine
(data-paths
 (registers
  ((name p)
   (buttons ((name p<-t1) (source (register t1)))
            ((name p<-1) (source (constant 1)))))
  ((name c)
   (buttons ((name c<-t2) (source (register t2)))
            ((name c<-1) (source (constant 1)))))
  ((name t1)
   (buttons ((name t1<-m) (source (operation *)))))
  ((name t2)
   (buttons ((name t2<-s) (source (operation +))))))
 (operations
  ((name *) (inputs (register p) (register c)))
  ((name +) (inputs (register c) (constant 1)))
  ((name >) (inputs (register c) (constant n)))))
(controller
 (p<-1)
 (c<-1)
 test-c
 (test >)
 (branch (label fact-done))
 (t1<-m)
 (p<-t1)
 (t2<-s)
 (c<-t2)
 (goto (label test-c))
 fact-done)

; convinient description
(controller
 (assign p (const 1))
 (assign c (const 1))
 test-c
 (test (op >) (reg c) (const n))
 (branch (label fact-done))
 (assign t1 (op *) (reg p) (reg c))
 (assign p (reg t1))
 (assign t2 (op +) (reg c) (const 1))
 (assign c (reg t2))
 (goto (label test-c))
 fact-done)

















