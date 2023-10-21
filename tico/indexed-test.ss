(import (micascheme) (tico indexed))

(check (equal? (term-evaluate "foo") "foo"))

(check
  (equal?
    (term-evaluate (application string-append (list "foo" "bar")))
    "foobar"))

(check
  (equal?
    (term-evaluate (abstraction "foo"))
    (abstraction "foo")))

(check
  (equal?
    (term-evaluate
      (application
        (abstraction (application string-append (list (variable 1) (variable 0))))
        (list "foo" "bar")))
    "foobar"))
