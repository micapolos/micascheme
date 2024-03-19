(import (check) (labs syntax))

; syntax-flatten
(check
  (equal?
    (map syntax->datum (syntax-flatten #'a))
    '(a)))

(check
  (equal?
    (map syntax->datum (syntax-flatten #'(begin)))
    '()))

(check
  (equal?
    (map syntax->datum (syntax-flatten #'(begin a b c)))
    '(a b c)))

(check
  (equal?
    (map syntax->datum (syntax-flatten #'(begin (begin a b) c)))
    '((begin a b) c)))
