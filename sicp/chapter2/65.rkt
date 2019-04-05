#lang sicp

(#%require rackunit)

(define (make-tree entry left right)
  (list entry left right))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1))
               (x2 (car set2)))
           (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                 ((> x1 x2) (cons x2 (union-set set1 (cdr set2))))
                 (else (cons x1 (union-set (cdr set1) set2))))))))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define x (make-tree 7
                     (make-tree 3
                                (make-tree 1 '() '())
                                (make-tree 5 '() '()))
                     (make-tree 9
                                '()
                                (make-tree 11 '() '()))))

(define y (make-tree 4
                     (make-tree 2 '() '())
                     (make-tree 8
                                (make-tree 6 '() '())
                                (make-tree 10
                                           '()
                                           (make-tree 12 '() '())))))

(define z (make-tree 6
                     (make-tree 3
                                (make-tree 1 '() '())
                                '())
                     (make-tree 9
                                (make-tree 8 '() '())
                                (make-tree 12 '() '()))))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons `() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))


(define (union-set-tree tree1 tree2)
  (list->tree (union-set (tree->list tree1)
                         (tree->list tree2))))

(check-equal? (tree->list (union-set-tree z z)) '(1 3 6 8 9 12))
(check-equal? (tree->list (union-set-tree x y)) '(1 2 3 4 5 6 7 8 9 10 11 12))

(define (intersection-set-tree tree1 tree2)
  (let ((set1 (tree->list tree1))
        (set2 (tree->list tree2)))
    (define (iter set1 set2)
      (cond ((null? set1) '())
            ((null? set2) '())
            (else
             (let ((x1 (car set1))
                   (x2 (car set2)))
               (cond ((= x1 x2) (cons x1 (iter (cdr set1) (cdr set2))))
                     ((> x1 x2) (iter set1 (cdr set2)))
                     (else (iter (cdr set1) set2)))))))
    (list->tree (iter set1 set2))))

(check-equal? (tree->list (intersection-set-tree x x)) (tree->list x))
(check-equal? (tree->list (intersection-set-tree x z)) '(1 3 9))
(check-equal? (tree->list (intersection-set-tree y z)) '(6 8 12))
