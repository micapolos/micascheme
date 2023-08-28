(import (micascheme) (monad))

; monad-return

(check 
  (equal? 
    (monad-return option-monad "foo") 
    "foo"))

; monad-bind

(check 
  (equal? 
    (monad-bind option-monad #f
      (lambda ($value) 
        (string-append $value "bar"))) 
    #f))

(check 
  (equal? 
    (monad-bind option-monad "foo" 
      (lambda ($value) 
        (string-append $value "bar"))) 
    "foobar"))

(check 
  (equal? 
    (monad-bind option-monad #f 
      (lambda ($value) #f))
    #f))

(check 
  (equal? 
    (monad-bind option-monad "foo" 
      (lambda ($value) #f))
    #f))

; monad-map

(check 
  (equal? 
    (monad-map option-monad "foo"
      (lambda ($value) 
        (string-append $value "bar")))
    "foobar"))

(check 
  (equal? 
    (monad-map option-monad #f
      (lambda ($value) 
        (string-append $value "bar")))
    #f))

; monad-sequence

(check (equal? (monad-sequence option-monad (list)) (list)))
(check (equal? (monad-sequence option-monad (list "a" "b" "c")) (list "a" "b" "c")))
(check (equal? (monad-sequence option-monad (list "a" #f "c")) #f))

; monad-lift

(check (equal? (monad-lift option-monad string-append "a" "b") "ab"))
(check (equal? (monad-lift option-monad string-append "a" #f) #f))
(check (equal? (monad-lift option-monad string-append #f "b") #f))
(check (equal? (monad-lift option-monad string-append #f #f) #f))
