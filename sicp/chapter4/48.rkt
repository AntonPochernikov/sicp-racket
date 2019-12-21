#lang racket

(require sicp)

(define nouns '(noun student professor cat class))
(define adverbs '(is are am do does))
(define verbs '(verb studies lectures eats sleeps))
(define adjectives '(adjective smart ambitios lazy fancy))
(define articles '(article the a))
(define prepositions '(prep for to in by with))

(define *unparsed* '())

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define (parse-simple-noun-phrase)
  (amb (list 'simple-noun-phrase
             (parse-word articles)
             (parse-word nouns))
       (list 'simple-noun-phrase
             (parse-word articles)
             (parse-word adjectives)
             (parse-word nouns))))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend
          (list 'noun-phrase
                noun-phrase
                (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(define (parse-simple-verb)
  (amb (parse-word verbs)
       (list 'simple-verv
             (parse-word adverbs)
             (parse-word verbs))))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend
          (list 'verb-phrase
                verb-phrase
                (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-verb)))

(define (parse-sentence)
  (list 'sentence (parse-noun-phrase)
        (parse-verb-phrase)))

(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*)) sent))








