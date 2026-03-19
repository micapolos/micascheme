(import (micascheme) (leo datum))

(check
  (equal? (->datum #t) '(atom true))
  (equal? (->datum #f) '(atom false))
  (equal? (->datum #\a) '(atom (char a)))
  (equal? (->datum #\1) '(atom (char 1)))
  (equal? (->datum #\space) '(atom (char space)))
  (equal? (->datum 123) 123)
  (equal? (->datum "foo") "foo")
  (equal? (->datum '()) '(atom null))
  (equal? (->datum '(1 2 3)) '(atom (list 1 2 3)))
  (equal? (->datum '(foo bar)) '(foo bar))
  (equal? (->datum '(#\a . #t)) '(atom (list (atom (char a)) . (atom true))))
  (equal? (->datum '(#\a #t)) '(atom (list (atom (char a)) (atom true))))
  (equal?
    (->datum (vector 1 #\a #t))
    '(atom (vector 1 (atom (char a)) (atom true))))
  (equal?
    (->datum (bytevector 1 2 3))
    '(atom (bytevector 1 2 3))))
