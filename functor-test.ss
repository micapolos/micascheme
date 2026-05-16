(import (scheme) (check) (functor))

(check
  (equal?
    (functor-map option-functor #f number->string)
    #f))

(check
  (equal?
    (functor-map option-functor 1 number->string)
    "1"))

(check
  (equal?
    (functor-map list-functor (list 1 2 3) number->string)
    (list "1" "2" "3")))

(check
  (equal?
    (functor-map vector-functor (vector 1 2 3) number->string)
    (vector "1" "2" "3")))

(check
  (equal?
    (functor-replace list-functor (list 1 2 3) "foo")
    (list "foo" "foo" "foo")))
