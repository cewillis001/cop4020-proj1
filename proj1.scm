; project 1 for cop 4020 Fall 2014
; by Caroline Willis

(define BOR
  (lambda (arg0 arg1)
    (cond
      (arg0   #t)
      (arg1   #t)
      (else   #f)
    )
  )
)

(define BAND
  (lambda (arg0 arg1)
    (cond
      ((equal? arg0 #f) #f)
      ((equal? arg1 #f) #f)
      (else             #t)
    )
  )
)

(define pos?
  (lambda (arg)
    (cond
      ((> arg 0) #t)
      (else      #f)
    )
  )
)

(define in?
  (lambda (arg lst)
    (cond 
      ((null? lst)             #f)
      ((equal? arg (car lst))  #t)
      ((in? arg (cdr lst))     #t)
      (else                    #f)
    )
  )   
)

(define reduce
  (lambda (operator list)
    (cond
      ((null? (cdr list))            (car list))
      (else   (operator (car list) (reduce operator (cdr list)))) 
    )
  )
)

(define filter
  (lambda (test list)
    (cond
      ((null? list)        '())
      ((test (car list))  (cons (car list) (filter test (cdr list))))
      (else               (filter test (cdr list)))
    )
  )
)


(define determiners '(a an the))

(define det?
  (lambda (arg)
   (cond
      ((in? arg determiners)   #t)
      (else                    #f)  
    )
  )
)

(define nouns '(apple bicycle car cow dog fox motorcycle path pie road truck))

(define noun?
  (lambda (arg)
    (cond
      ((in? arg nouns)  #t)
      (else             #f)
    )
  )
)

(define adjectives '(black brown fast hairy hot quick red slow))

(define adj?
  (lambda (arg)
    (cond
      ((in? arg adjectives)  #t)
      (else                  #f)
    )
  )
)

(define verbs '(commutes destroys drives eats jumps makes occupies rides stops travels walks))

(define verb?
  (lambda (arg)
    (cond
      ((in? arg verbs)  #t)
      (else             #f)
    )
  )
)

(define prepositions '(around at of on over to under))

(define prep?
  (lambda (arg)
    (cond
      ((in? arg prepositions)  #t)
      (else                    #f)
    )
  )
)

(define OK
  (lambda (fragment)
    (cond
      ((null? fragment)                                               #f)
      ((> (/ (length (filter adj? fragment)) (length fragment)) .25)  #f)
      (else                                                           #t)
    )
  )
)

(define det
  (lambda (fragment)
    (cond
      ((null? fragment)       '())
      ((det? (car fragment))  (cdr fragment))
      (else                   '())
    )
  )
)

(define noun
  (lambda (fragment)
    (cond
      ((null? fragment)       '())
      ((noun? (car fragment)) (cdr fragment))
      (else                   '())
    )
  )
)

(define verb
  (lambda (fragment)
    (cond
      ((null? fragment)       '())
      ((verb? (car fragment)) (cdr fragment))
      (else                   '())
    )
  )
)

(define adj
  (lambda (fragment)
    (cond
      ((null?       fragment) '())
      ((adj?  (car fragment)) (cdr fragment))
      (else                   '())
    )
  )
)

(define prep
  (lambda (fragment)
    (cond
      ((null?  fragment)      '())
      ((prep? (car fragment)) (cdr fragment))
      (else                   '())
    )
  )
)

; questions

; (reduce BOR (map det? '(hot red car)))
;
; Returns #f. It calculates this result by using the map function 
; to generate a list of the results of det? as applied to each 
; element in the list (hot red car). Since none are determiners, 
; the list is (#f #f #f). Then reduce is called, which applies BOR 
; to that list; since there are no #t, each comparison returns #f 
; resulting in the final reduced #f.

; (reduce BOR (map det? '(the red car is a hot dog)))

; Returns #t. If calculates this result by using the map function 
; to generate a list of the results of det? as applied to each 
; element in the list (the red car is a hot dog). The resulting 
; list is (#t #f #f #f #t #f #f). Then reduce is called, which 
; applies BOR to that list; since there is at least one #t, the 
; reduced BOR is #t.


(define nounphrase2
  (lambda (fragment)
    (cond
      ((null? fragment)              #f)
      ((equal? #f fragment)          #f)
      ((noun? (car fragment))        (noun fragment))
      ((adj? (car fragment))         (nounphrase2 (adj fragment)))
      (else                           #f)
    )
  )
)

(define nounphrase1
  (lambda (fragment)
    (cond
      ((null? fragment)                      #f)
      ((equal? #f fragment)                  #f)
      ((det? (car fragment)) 
        (cond
          ((null? (cdr fragment))                    #f)
          ((equal? #f (cdr fragment))                #f)
          ((equal? #f (nounphrase2 (cdr fragment)))  #f)
          (else                                      (nounphrase2 (det fragment)))
        )
      )
      ((equal? #f (nounphrase2 fragment))   #f)
      (else                                 (nounphrase2 fragment))
    )
  )
)

(define verbphrase
  (lambda (fragment)
    (cond
      ((null? fragment)           #f)
      ((verb? (car fragment))
        (cond
          ((null? (cdr fragment))         (verb fragment))
          ((prep? (car (cdr fragment)))
            (cond
              ((null? (car (cdr (cdr fragment))))             (prep (verb fragment)))
              ((equal? #f (nounphrase1 (cdr (cdr fragment)))) (prep (verb fragment)))
              (else                                           (nounphrase1 (prep (verb fragment))))
            )
          )
          ((equal? #f (nounphrase1 (cdr fragment)))  (verb fragment))
          (else                                      (nounphrase1 (verb fragment)))
        )
      )
      (else                        #f)
    )
  )
)

(define sentence
  (lambda (fragment)
    (cond
      ((null? fragment)                    #f)
      ((equal? #f (nounphrase1 fragment))  #f)
      (else
        (cond
          ((null? (cdr fragment))                           #f)
          ((equal? #f (verbphrase (nounphrase1 fragment)))  #f)
          (else                                             (verbphrase (nounphrase1 fragment)))
        )
      )
    )
  )
)