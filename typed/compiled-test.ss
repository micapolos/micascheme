(import (micascheme) (typed scope) (typed compiled))

(check
  (equal?
    (compiled-value
      (environment '(scheme))
      (compiled
        (scope
          (foo "foo")
          (bar "bar"))
        '(string-append foo bar)))
    "foobar"))

(check
  (equal?
    (value-compiled "foo")
    (compiled (scope (tmp_0 "foo")) 'tmp_0)))

(check
  (equal?
    (compiled-bind
      (compiled (scope (foo "foo")) 'foo)
      "bar"
      (lambda ($datum $tmp)
        `(let ((,$tmp "bar")) (string-append foo tmp_0))))
    (compiled
      (scope (foo "foo") (tmp_0 "bar"))
      '(let ((tmp_0 "bar")) (string-append foo tmp_0)))))

