(import (typico base) (typico fragment))

(check
  (equal?
    (fragment-bind
      (lambda ($obj)
        (fragment `(b ,$obj) (list '(import b) '(import c))))
      (fragment 'a (list '(import a) '(import b))))
    (fragment
      '(b a)
      (list '(import a) '(import b) '(import c)))))
