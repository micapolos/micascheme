(import (micascheme) (leo datum))

(check (equal? (atom->datum #t) 'true))
(check (equal? (atom->datum #f) 'false))

(check (equal? (atom->datum #\a) '(char a)))
(check (equal? (atom->datum #\1) '(char 1)))
(check (equal? (atom->datum #\space) '(char space)))

(check (equal? (atom->datum 123) 123))
(check (equal? (atom->datum "foo") "foo"))

(check
  (equal?
    (atom->datum (vector 1 #\a #t))
    '(vector 1 (char a) true)))

(check
  (equal?
    (atom->datum (bytevector 1 2 3))
    '(bytevector 1 2 3)))
