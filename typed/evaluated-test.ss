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

; evaluated-compiled

(check
  (equal?
    (evaluated-compiled "foo")
    (compiled (scope (tmp_0 "foo")) 'tmp_0)))

(check
  (equal?
    (evaluated-compiled (thunk 2 (compiled (scope (foo "foo")) 'foo)))
    (compiled (scope (foo "foo")) 'foo)))

; combine-evaluated-list

(check
  (equal?
    (combine-evaluated-list
      (environment '(scheme))
      (list "foo" "bar")
      (lambda ($datums)
        `(string-append ,@$datums)))
    "foobar"))

(check
  (equal?
    (combine-evaluated-list
      (environment '(scheme))
      (list
        (thunk 3
          (compiled
            (scope (foo-1 "foo-1") (foo-2 "foo-2"))
            '(string-append foo-1 foo-2)))
        ", "
        (thunk 5
          (compiled
            (scope (bar-1 "bar-1") (bar-2 "bar-2"))
            '(string-append bar-1 bar-2))))
      (lambda ($datums)
        `(string-append ,@$datums)))
    (thunk 5
      (compiled
        (scope (foo-1 "foo-1") (foo-2 "foo-2") (tmp_0 ", ") (bar-1 "bar-1") (bar-2 "bar-2"))
        '(string-append (string-append foo-1 foo-2) tmp_0 (string-append bar-1 bar-2))))))


