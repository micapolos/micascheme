(import (micascheme) (typed thunk) (typed compiled))

; thunk-promote

(check
  (equal?
    (thunk-promote
      (environment '(scheme))
      (thunk 5
        (compiled
          (stack (cons 'foo "foo"))
          '(string-append foo "bar")))
      2)
    (thunk 3
      (compiled
        (stack (cons 'foo "foo"))
        '(string-append foo "bar")))))

(check
  (equal?
    (thunk-promote
      (environment '(scheme))
      (thunk 5
        (compiled
          (stack (cons 'foo "foo"))
          '(string-append foo "bar")))
      5)
    (thunk 0
      (compiled
        (stack (cons 'foo "foo"))
        '(string-append foo "bar")))))

(check
  (equal?
    (thunk-promote
      (environment '(scheme))
      (thunk 5
        (compiled
          (stack (cons 'foo "foo"))
          '(string-append foo "bar")))
      6)
    "foobar"))

; thunk-bind

(check
  (equal?
    (thunk-bind
      (thunk 2
        (compiled
          (stack (cons 'foo "foo"))
          'foo))
      "bar"
      (lambda ($datum $tmp)
        `(string-append ,$datum ,$tmp)))
    (thunk 2
        (compiled
          (stack (cons 'foo "foo") (cons 'tmp_0 "bar"))
          '(string-append foo tmp_0)))))

