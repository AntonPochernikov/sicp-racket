#lang sicp

(#%require rackunit)

(define (enumerate-interval from to)
  (if (> from to)
      nil
      (cons from (enumerate-interval (inc from) to))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap op seq)
  (accumulate append nil (map op seq)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (filter predicate? seq)
  (cond ((null? seq) nil)
        ((predicate? (car seq)) (cons (car seq) (filter predicate? (cdr seq))))
        (else (filter predicate? (cdr seq)))))

(define (square x) (* x x))
(define (divides? a b) (= (remainder a b) 0))

(define (smallest-divisor n)
  (define (iter counter)
    (cond ((> (square counter) n) n)
          ((divides? n counter) counter)
          (else (iter (+ counter 1)))))
  (iter 2))

(define (prime? n) (= n (smallest-divisor n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(check-equal? (prime-sum-pairs 6) '((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11)))
