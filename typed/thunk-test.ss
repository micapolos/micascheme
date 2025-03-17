(import (micascheme) (typed scope) (typed thunk) (typed compiled))

; thunk-promote

(check
  (equal?
    (thunk-promote
      (environment '(scheme))
      (thunk 5
        (compiled
          (scope (foo "foo"))
          '(string-append foo "bar")))
      2)
    (thunk 3
      (compiled
        (scope (foo "foo"))
        '(string-append foo "bar")))))

(check
  (equal?
    (thunk-promote
      (environment '(scheme))
      (thunk 5
        (compiled
          (scope (foo "foo"))
          '(string-append foo "bar")))
      5)
    (thunk 0
      (compiled
        (scope (foo "foo"))
        '(string-append foo "bar")))))

(check
  (equal?
    (thunk-promote
      (environment '(scheme))
      (thunk 5
        (compiled
          (scope (foo "foo"))
          '(string-append foo "bar")))
      6)
    "foobar"))

; thunk-bind

(check
  (equal?
    (thunk-bind
      (thunk 2
        (compiled
          (scope (foo "foo"))
          'foo))
      "bar"
      (lambda ($datum $tmp)
        `(string-append ,$datum ,$tmp)))
    (thunk 2
        (compiled
          (scope (foo "foo") (tmp_0 "bar"))
          '(string-append foo tmp_0)))))

; combine-thunks

(check
  (equal?
    (combine-thunks
      (list
        (thunk 3
          (compiled
            (scope (foo-1 "foo-1") (foo-2 "foo-2"))
            '(string-append foo-1 foo-2)))
        (thunk 5
          (compiled
            (scope (bar-1 "bar-1") (bar-2 "bar-2"))
            '(string-append bar-1 bar-2))))
      (lambda ($datums)
        `(string-append ,@$datums)))
    (thunk 5
      (compiled
        (scope (foo-1 "foo-1") (foo-2 "foo-2") (bar-1 "bar-1") (bar-2 "bar-2"))
        '(string-append (string-append foo-1 foo-2) (string-append bar-1 bar-2))))))

