(import (typico base) (typico fragment))

(check
  (equal?
    (fragment-bind
      (lambda ($obj)
        (fragment
          (list '(import b) '(import c))
          `(b ,$obj)))
      (fragment
        (list '(import a) '(import b))
        'a))
    (fragment
      (list '(import a) '(import b) '(import c))
      '(b a))))
