#lang sicp

(define (find proc L)
  (cond ((null? L) false)
        ((proc (car L)) (car L))
        (else (find proc (cdr L)))))

(define (find-by-key key L)
  (find (lambda (item)
          (eq? (get-key item) key))
        L))
(define (find-by-value value L)
  (find (lambda (item)
          (eq? (get-value item) value))
        L))

(define (reduce iteratee L initial)
  (define (iter acc rest)
    (if (null? rest)
        acc
        (iter (iteratee acc (car rest)) (cdr rest))))
  (iter initial L))

(define (make-node key value) (cons key value))
(define (get-key node) (car node))
(define (get-value node) (cdr node))

(define (find-closest-path graph start-name finish-name)
  (define costs (list (make-node start-name 0) (make-node finish-name +inf.0)))
  (define parents (list (make-node finish-name nil)))
  (define passed '())
  (define (is-passed? node)
    (find-by-key (get-key node) passed))
  (define (add-passed! node) (set! passed (cons node passed)))

  (define (find-lowest-cost-node)
    (reduce (lambda (acc item)
              (if (is-passed? item)
                  acc
                  (cond ((eq? acc false) item)
                        ((< (get-value item) (cdr acc)) item)
                        (else acc))))
            costs
            false))

  (define (path-to-list parents)
    (define (iter acc current)
      (if current
          (let ((path-to (get-value current)))
            (iter (cons path-to acc)
                  (find-by-key path-to parents)))
          acc))
    (let ((finish (find-by-key finish-name parents)))
      (if (not finish)
          '()
          (iter '(finish) finish))))

  (define (iter)
    (let ((node (find-lowest-cost-node)))
      (if node
          (let ((node-name (get-key node))
                (node-cost (get-value node)))
            (let ((neighbours
                   (find-by-key (get-key node) graph)))
              (for-each (lambda (neighbour)
                          (let ((new-cost (+ node-cost (get-value neighbour)))
                                (old-cost (find-by-key (get-key neighbour) costs))
                                (neighbour-name (get-key neighbour)))
                            (if (or (eq? old-cost false)
                                    (> (cdr old-cost) new-cost))
                                (begin
                                  (let ((parent
                                         (find-by-key neighbour-name parents)))
                                    (if parent
                                        (set-cdr! parent node-name)
                                        (set! parents
                                              (cons (make-node neighbour-name
                                                               node-name)
                                                    parents))))
                                  (cond ((eq? old-cost false)
                                         (set! costs
                                               (cons (make-node neighbour-name
                                                                new-cost)
                                                     costs)))
                                        ((> (cdr old-cost) new-cost)
                                         (set-cdr! old-cost new-cost)))))))
                        (cdr neighbours))
              (add-passed! node)
              (iter)))
          (cons (get-value (find-by-key finish-name costs))
                (path-to-list parents)))))
  (iter))

(define graph (list (make-node 'start (list (make-node 'a 6) (make-node 'b 2)))
                    (make-node 'a (list (make-node 'finish 1)))
                    (make-node 'b (list (make-node 'a 3) (make-node 'finish 5)))
                    (make-node 'finish '())))

(display (find-closest-path graph 'start 'finish))

(define graph-2 (list (make-node 'start (list (make-node 'a 5) (make-node 'b 2)))
                      (make-node 'a (list (make-node 'c 4) (make-node 'd 2)))
                      (make-node 'b (list (make-node 'a 8) (make-node 'd 7)))
                      (make-node 'c (list (make-node 'finish 3) (make-node 'd 6)))
                      (make-node 'd (list (make-node 'finish 1)))
                      (make-node 'finish '())))
(newline)
(display (find-closest-path graph-2 'start 'finish))

  