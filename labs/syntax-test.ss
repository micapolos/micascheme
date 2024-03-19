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

; syntax-flatten-rec
(check
  (equal?
    (map syntax->datum (syntax-flatten-rec #'a))
    '(a)))

(check
  (equal?
    (map syntax->datum (syntax-flatten-rec #'(begin)))
    '()))

(check
  (equal?
    (map syntax->datum (syntax-flatten-rec #'(begin a b c)))
    '(a b c)))

(check
  (equal?
    (map syntax->datum (syntax-flatten-rec #'(begin (begin a b) (begin c))))
    '(a b c)))

(check
  (equal?
    (map syntax->datum (syntax-flatten-rec #'(begin a (b (begin c)))))
    '(a (b (begin c)))))
