(import (micascheme) (monad))

; monad-pure

(check 
  (equal? 
    (monad-pure option-monad "foo") 
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

; listing monad

(check
  (equal?
    (listing-run
      (monad-bind listing-monad (monad-pure listing-monad "foo")
        (lambda ($foo)
          (monad-map listing-monad (monad-pure listing-monad "bar")
            (lambda ($bar)
              (string-append $foo $bar))))))
    (cons "foobar" (list "foobar" "bar" "foo"))))

; monadic

(check
  (equal?
    (listing-run (monadic listing-monad (listing "foo")))
    (cons "foo" (list "foo"))))

(check
  (equal?
    (listing-run (monadic listing-monad (pure "foo")))
    (cons "foo" (list "foo"))))

(check
  (equal?
    (listing-run (monadic listing-monad (lets (pure "foo"))))
    (cons "foo" (list "foo"))))

(define-monadic (plus $a $b)
  (pure (+ $a $b)))

(check
  (equal?
    (listing-run
      (monadic listing-monad
        (lets
          ($foo (pure "foo"))
          ($bar (pure "bar"))
          (pure (string-append $foo $bar)))))
    (cons "foobar" (list "foobar" "bar" "foo"))))
