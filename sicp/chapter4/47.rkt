#lang racket

(require sicp)

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend
          (list 'verb-phrase
                verb-phrase
                (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

; SOLUTION
; This will cause the infinite loop if we go into second
; branch of amb
(define (parse-verb-phrase)
  (amb (parse-word verbs)
       (list 'verb-phrase
             (parse-verb-phrase)
             (parse-prepositional-phrase))))









