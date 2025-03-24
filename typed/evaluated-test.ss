(import (micascheme) (typed evaluated) (typed thunk) (typed compiled) (typed scope) (typed combo))

; evaluated-promote

(check
  (equal?
    (evaluated-promote
      (environment '(scheme))
      (combo "foo" "foo")
      2)
    (combo "foo" "foo")))

(check
  (equal?
    (evaluated-promote
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
    (evaluated-promote
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
    (evaluated-promote
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

; evaluated-max-index?

(check
  (equal?
    (evaluated-max-index? (combo 123 'num))
    #f))

(check
  (equal?
    (evaluated-max-index?
      (thunk 3 (compiled (scope) 'foo)))
    3))

(check
  (equal?
    (evaluated-list-max-index? (list (combo 123 'num1) (combo 124 'num2)))
    #f))

(check
  (equal?
    (evaluated-list-max-index?
      (list
        (combo 123 'num1)
        (thunk 5 (compiled (scope) 'foo))
        (thunk 3 (compiled (scope) 'bar))))
    5))

; evaluated-compiled

(check
  (equal?
    (evaluated-compiled (combo "foo" '(string #\f #\o #\o)))
    (compiled (scope) '(string #\f #\o #\o))))

(check
  (equal?
    (evaluated-compiled (thunk 2 (compiled (scope (foo "foo")) 'foo)))
    (compiled (scope (foo "foo")) 'foo)))

; combine-evaluated-list

(check
  (equal?
    (combine-evaluated-list
      (environment '(micascheme))
      (list
        (combo "foo" '(string #\f #\o #\o))
        (combo "bar" '(string #\b #\a #\r)))
      (lambda ($datums)
        `(string-append ,@$datums)))
    (combo
      "foobar"
      '(lets
        (string-append
          (string #\f #\o #\o)
          (string #\b #\a #\r))))))

(check
  (equal?
    (combine-evaluated-list
      (environment '(scheme))
      (list
        (combo string-append 'string-append)
        (thunk 3
          (compiled
            (scope
              (foo-1 (combo "1" '(string #\1)))
              (foo-2 (combo "2" '(string #\2))))
            '(string-append foo-1 foo-2)))
        (combo ", " '(string #\, #\space))
        (thunk 5
          (compiled
            (scope
              (foo-3 (combo "3" '(string #\3)))
              (foo-4 (combo "4" '(string #\4))))
            '(string-append foo-3 foo-4))))
      (lambda ($datums)
        `(,@$datums)))
    (thunk 5
      (compiled
        (scope
          (foo-1 (combo "1" '(string #\1)))
          (foo-2 (combo "2" '(string #\2)))
          (foo-3 (combo "3" '(string #\3)))
          (foo-4 (combo "4" '(string #\4))))
        '(string-append
          (string-append foo-1 foo-2)
          (string #\, #\space)
          (string-append foo-3 foo-4))))))
