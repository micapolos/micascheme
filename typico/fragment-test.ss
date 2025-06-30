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
