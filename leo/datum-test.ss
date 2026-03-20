(import (micascheme) (leo datum))

(check
  (equal? (->datum #t) (string->symbol "#true"))
  (equal? (->datum #f) (string->symbol "#false"))
  (equal? (->datum #\a) `(,(string->symbol "#char") a))
  (equal? (->datum #\1) `(,(string->symbol "#char") 1))
  (equal? (->datum #\space) `(,(string->symbol "#char") space))
  (equal? (->datum 123) 123)
  (equal? (->datum "foo") "foo")
  (equal? (->datum '()) (string->symbol "#null"))
  (equal? (->datum '(1 2 3)) '(: 1 2 3))
  (equal? (->datum '(foo bar)) '(foo bar))
  (equal? (->datum '(#\a . #t)) `(: (,(string->symbol "#char") a) . ,(string->symbol "#true")))
  (equal? (->datum '(#\a #t)) `(: (,(string->symbol "#char") a) ,(string->symbol "#true")))
  (equal?
    (->datum (vector 1 #\a #t))
    `(,(string->symbol "#vector")
      1
      (,(string->symbol "#char") a)
      ,(string->symbol "#true")))
  (equal?
    (->datum (bytevector 1 2 3))
    `(,(string->symbol "#bytevector") 1 2 3)))
