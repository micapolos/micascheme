(import (micascheme) (typed compiled) (typed evaluate))

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
