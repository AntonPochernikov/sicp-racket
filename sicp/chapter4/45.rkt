#lang racket

(require sicp)

; 1 The professor that has a cat, lectures to the student, who is in the class
(sentence
 (simple-noun-phrase (article the) (noun professor))
 (verb-phrase
  (verb-phrase
   (verb-phrase
    (verb lectures)
    (prep-phrase
     (prep to)
     (simple-noun-phrase (article the) (noun student))))
   (prep-phrase
    (prep in)
    (simple-noun-phrase (article the) (noun class))))
  (prep-phrase
   (prep with)
   (simple-noun-phrase (article the) (noun cat)))))

; 2 The professor lectures to the student with the cat, that is in the class, 
(sentence
 (simple-noun-phrase (article the) (noun professor))
 (verb-phrase
  (verb-phrase
   (verb lectures)
   (prep-phrase
    (prep to)
    (simple-noun-phrase (article the) (noun student))))
  (prep-phrase
   (prep in)
   (noun-phrase
    (simple-noun-phrase (article the) (noun class))
    (prep-phrase
     (prep with)
     (simple-noun-phrase (article the) (noun cat)))))))

; 3 The professor and the cat lecture to the student in the class
(sentence
 (simple-noun-phrase (article the) (noun professor))
 (verb-phrase
  (verb-phrase
   (verb lectures)
   (prep-phrase
    (prep to)
    (noun-phrase
     (simple-noun-phrase (article the) (noun student))
     (prep-phrase
      (prep in)
      (simple-noun-phrase (article the) (noun class))))))
  (prep-phrase
   (prep with)
   (simple-noun-phrase (article the) (noun cat)))))

; 4 The professor lectures to the student with the cat in the class
(sentence
 (simple-noun-phrase (article the) (noun professor))
 (verb-phrase
  (verb lectures)
  (prep-phrase
   (prep to)
   (noun-phrase
    (noun-phrase
     (simple-noun-phrase (article the) (noun student))
     (prep-phrase
      (prep in)
      (simple-noun-phrase (article the) (noun class))))
    (prep-phrase
     (prep with)
     (simple-noun-phrase (article the) (noun cat)))))))

; 5 The professor lectures to the student in the class that has cat
(sentence
 (simple-noun-phrase (article the) (noun professor))
 (verb-phrase
  (verb lectures)
  (prep-phrase
   (prep to)
   (noun-phrase
    (simple-noun-phrase (article the) (noun student))
    (prep-phrase
     (prep in)
     (noun-phrase
      (simple-noun-phrase (article the) (noun class))
      (prep-phrase
       (prep with)
       (simple-noun-phrase (article the) (noun cat)))))))))














