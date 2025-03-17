(import (micascheme) (typed evaluated) (typed thunk) (typed compiled) (typed scope))

(check
  (equal?
    (evaluated-max-index? 123)
    #f))

(check
  (equal?
    (evaluated-max-index?
      (thunk 3 (compiled (scope) 'foo)))
    3))

(check
  (equal?
    (evaluated-list-max-index? (list 123 124))
    #f))

(check
  (equal?
    (evaluated-list-max-index?
      (list
        123
        (thunk 5 (compiled (scope) 'foo))
        (thunk 3 (compiled (scope) 'bar))))
    5))

(check
  (equal?
    (evaluated-bind
      (environment '(scheme))
      "foo"
      "bar"
      (lambda ($datum $tmp)
        `(string-append ,$datum ,$tmp)))
    "foobar"))

(check
  (equal?
    (evaluated-bind
      (environment '(scheme))
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

