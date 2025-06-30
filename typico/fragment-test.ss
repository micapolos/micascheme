(import (typico base) (typico fragment))

; fragment-with

(check
  (equal?
    (fragment-with (+ 1 2))
    (fragment (list) '(+ 1 2))))

(check
  (equal?
    (fragment-with (import (scheme) (data)) (+ 1 2))
    (fragment (list '(scheme) '(data)) '(+ 1 2))))

; fragment-bind

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

