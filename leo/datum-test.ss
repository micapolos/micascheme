(import (micascheme) (leo datum))

(check
  (equal? (atom->datum #t) 'true)
  (equal? (atom->datum #f) 'false)
  (equal? (atom->datum #\a) '(char a))
  (equal? (atom->datum #\1) '(char 1))
  (equal? (atom->datum #\space) '(char space))
  (equal? (atom->datum 123) 123)
  (equal? (atom->datum "foo") "foo")
  (equal?
    (atom->datum (vector 1 #\a #t))
    '(vector 1 (char a) true))
  (equal?
    (atom->datum (bytevector 1 2 3))
    '(bytevector 1 2 3)))
