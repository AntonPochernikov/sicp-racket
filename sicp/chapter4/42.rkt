#lang racket

(require sicp)

; SOLUTION
; I don't have access to amb-evaluator
; so i solved it with filtered permutations.

(define (filter pred L)
  (cond [(null? L) L]
        [(pred (car L))
         (cons (car L) (filter pred (cdr L)))]
        [else
         (filter pred (cdr L))]))

(define (without item L)
  (filter (lambda (x) (not (eq? x item)))
          L))

(define (ref item L)
  (cond [(null? L) '()]
        [(eq? item (car L)) 1]
        [else
         (+ 1 (ref item (cdr L)))]))

(define names '(betty ethel joan kitty mary))

(define (reduce iteratee init L)
  (if (null? L)
      init
      (reduce iteratee
              (iteratee init (car L))
              (cdr L))))

(define (flatmap proc L)
  (reduce (lambda (acc item)
            (append acc (proc item)))
          '()
          L))

(define (permutate L)
  (if (null? L)
      (list '())
      (flatmap
       (lambda (current)
         (map (lambda (combination)
                (cons current combination))
              (permutate (without current L))))
       L)))

(define (filter-map predicates L)
  (if (null? predicates)
      L
      (filter-map (cdr predicates)
                  (filter (car predicates) L))))

(define (xor exp1 exp2)
  (and (or exp1 exp2) (not (and exp1 exp2))))

(display
 (filter-map
  (list
   (lambda (names)
     (xor (eq? (ref 'kitty names) 2)
          (eq? (ref 'betty names) 3)))
   (lambda (names)
     (xor (eq? (ref 'ethel names) 1)
          (eq? (ref 'joan names) 2)))
   (lambda (names)
     (xor (eq? (ref 'joan names) 3)
          (eq? (ref 'ethel names) 5)))
   (lambda (names)
     (xor (eq? (ref 'kitty names) 2)
          (eq? (ref 'mary names) 4)))
   (lambda (names)
     (xor (eq? (ref 'mary names) 4)
          (eq? (ref 'betty names) 1))))
  (permutate names)))

; (kitty joan betty mary ethel)

(define (liars-puzzle)
  (let ((kitty (amb 1 2 3 4 5))
        (joan (amb 1 2 3 4 5))
        (betty (amb 1 2 3 4 5))
        (mary (amb 1 2 3 4 5))
        (ethel (amb 1 2 3 4 5)))
    (require
      (distinct? (list kitty joan betty mary ethel)))
    (require (xor (= kitty 2) (= betty 3)))
    (require (xor (= ethel 1) (= joan 2)))
    (require (xor (= joan 3) (= ethel 5)))
    (require (xor (= kitty 2) (= mary 4)))
    (require (xor (= mary 4) (= betty 1)))
    (list (list 'kitty kitty)
          (list 'joan joan)
          (list 'betty betty)
          (list 'mary mary)
          (list 'ethel ethel))))












