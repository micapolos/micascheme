(import (micascheme) (leo datum))

(check
  (equal? (->datum #t) '(the true))
  (equal? (->datum #f) '(the false))
  (equal? (->datum #\a) '(the (char a)))
  (equal? (->datum #\1) '(the (char 1)))
  (equal? (->datum #\space) '(the (char space)))
  (equal? (->datum 123) 123)
  (equal? (->datum "foo") "foo")
  (equal? (->datum '()) '(the null))
  (equal? (->datum '(1 2 3)) '(the (list 1 2 3)))
  (equal? (->datum '(foo bar)) '(foo bar))
  (equal? (->datum '(#\a . #t)) '(the (list (the (char a)) . (the true))))
  (equal? (->datum '(#\a #t)) '(the (list (the (char a)) (the true))))
  (equal?
    (->datum (vector 1 #\a #t))
    '(the (vector 1 (the (char a)) (the true))))
  (equal?
    (->datum (bytevector 1 2 3))
    '(the (bytevector 1 2 3))))
