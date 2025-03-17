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
    (compiled (scope) "foo")))

(check
  (equal?
    (value-compiled (cons "foo" "bar"))
    (compiled (scope (tmp_0 (cons "foo" "bar"))) 'tmp_0)))

(check
  (equal?
    (combine-compiled-list
      (list
        (compiled
          (scope (foo-1 "foo-1") (foo-2 "foo-2"))
          '(string-append foo-1 foo-2))
        (compiled
          (scope (bar-1 "bar-1") (bar-2 "bar-2"))
          '(string-append bar-1 bar-2)))
      (lambda ($datums)
        `(string-append ,@$datums)))
    (compiled
      (scope (foo-1 "foo-1") (foo-2 "foo-2") (bar-1 "bar-1") (bar-2 "bar-2"))
      '(string-append (string-append foo-1 foo-2) (string-append bar-1 bar-2)))))
