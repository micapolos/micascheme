(import (typico base) (typico fragment))

; fragment-with

(check
  (equal?
    (fragment (+ 1 2))
    (make-fragment (list) '(+ 1 2))))

(check
  (equal?
    (fragment (import (scheme) (data)) (+ 1 2))
    (make-fragment (list '(scheme) '(data)) '(+ 1 2))))

; fragment-bind

(check
  (equal?
    (fragment-bind
      (lambda ($obj)
        (make-fragment
          (list '(import b) '(import c))
          `(b ,$obj)))
      (make-fragment
        (list '(import a) '(import b))
        'a))
    (make-fragment
      (list '(import a) '(import b) '(import c))
      '(b a))))

(check
  (equal?
    (fragment-eval
      (fragment (import (scheme)) (+ 1 2)))
    3))

(check
  (equal?
    (fragment-append
      (fragment (import i1 i2) v1)
      (fragment (import i2 i3) v2)
      (fragment (import i2 i3 i4) v3))
    (fragment (import i1 i2 i3 i4) (v1 v2 v3))))
