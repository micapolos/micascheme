(import (micascheme) (typed scope) (typed thunk) (typed compiled) (typed combo))

; thunk-promote

(check
  (equal?
    (thunk-promote
      (environment '(scheme))
      (thunk 5
        (compiled
          (scope (foo (combo "foo" '(string #\f #\o #\o))))
          '(string-append foo "bar")))
      2)
    (thunk 3
      (compiled
        (scope (foo (combo "foo" '(string #\f #\o #\o))))
        '(string-append foo "bar")))))

(check
  (equal?
    (thunk-promote
      (environment '(scheme))
      (thunk 5
        (compiled
          (scope (foo (combo "foo" '(string #\f #\o #\o))))
          '(string-append foo "bar")))
      5)
    (thunk 0
      (compiled
        (scope (foo (combo "foo" '(string #\f #\o #\o))))
        '(string-append foo "bar")))))

(check
  (equal?
    (thunk-promote
      (environment '(scheme))
      (thunk 5
        (compiled
          (scope (foo (combo "foo" '(string #\f #\o #\o))))
          '(string-append foo "bar")))
      6)
    (combo
      "foobar"
      '(lets
        (foo (string #\f #\o #\o))
        (string-append foo "bar")))))

; combine-thunks

(check
  (equal?
    (combine-thunks
      (list
        (thunk 3
          (compiled
            (scope
              (foo-1 (combo "1" '(string #\1)))
              (foo-2 (combo "2" '(string #\2))))
            '(string-append foo-1 foo-2)))
        (thunk 5
          (compiled
            (scope
              (foo-3 (combo "3" '(string #\3)))
              (foo-4 (combo "4" '(string #\4))))
            '(string-append bar-1 bar-2))))
      (lambda ($datums)
        `(string-append ,@$datums)))
    (thunk 5
      (compiled
        (scope
          (foo-1 (combo "1" '(string #\1)))
          (foo-2 (combo "2" '(string #\2)))
          (foo-3 (combo "3" '(string #\3)))
          (foo-4 (combo "4" '(string #\4))))
        '(string-append
          (string-append foo-1 foo-2)
          (string-append bar-1 bar-2))))))

