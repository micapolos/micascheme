(import (micascheme) (leo datum))

(check
  (equal? (->datum #t) 'true)
  (equal? (->datum #f) 'false)
  (equal? (->datum #\a) '(char a))
  (equal? (->datum #\1) '(char 1))
  (equal? (->datum #\space) '(char space))
  (equal? (->datum 123) 123)
  (equal? (->datum "foo") "foo")
  (equal? (->datum '()) '())
  (equal? (->datum '(#\a . #t)) '((char a) . true))
  (equal? (->datum '(#\a #t)) '((char a) true))
  (equal?
    (->datum (vector 1 #\a #t))
    '(vector 1 (char a) true))
  (equal?
    (->datum (bytevector 1 2 3))
    '(bytevector 1 2 3)))
