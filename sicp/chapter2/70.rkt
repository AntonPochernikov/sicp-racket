#lang sicp

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (symbol-leaf leaf)
  (cadr leaf))
(define (weight-leaf leaf)
  (caddr leaf))
(define (leaf? obj)
  (eq? 'leaf (car obj)))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge trees)
  (if (null? (cdr trees))
      (car trees)
      (successive-merge
       (adjoin-set (make-code-tree (car trees)
                                   (cadr trees))
                   (cddr trees)))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (if (leaf? tree)
      '()
      (cond ((element-of-set? symbol (symbols (left-branch tree)))
             (cons 0 (encode-symbol symbol (left-branch tree))))
            ((element-of-set? symbol (symbols (right-branch tree)))
             (cons 1 (encode-symbol symbol (right-branch tree))))
            (else (error "symbol not found -- ENCODE-SYMBOL" symbol)))))

(define (element-of-set? x set) 
  (cond ((null? set) false) 
        ((equal? x (car set)) true) 
        (else (element-of-set? x (cdr set)))))

(define rock-alphabet '(('A 2)
                        ('BOOM 1)
                        ('GET 2)
                        ('JOB 2)
                        ('NA 16)
                        ('SHA 3)
                        ('YIP 9)
                        ('WAH 1)))

(define rock-tree (generate-huffman-tree rock-alphabet))
(define rock-message '('GET 'A 'JOB 
                       'SHA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 
                       'GET 'A 'JOB 
                       'SHA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 'NA
                       'WAH 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 
                       'SHA 'BOOM))
(define encoded-rock-message (encode rock-message rock-tree))
(display (length encoded-rock-message))
(newline)
; encoded message required 84 bits
; if we were using alphabet with fixed size, every symbol would be encoded with 3 digits
(display (length rock-message))
; rock message length is 36, so we would need 36 * 3 = 108 symbols
