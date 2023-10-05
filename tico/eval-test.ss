(import (micascheme) (tico eval))

(check
  (equal?
    (bindings-eval
      (list)
      `(string-append "foo" "bar"))
    "foobar"))

(check
  (equal?
    (bindings-eval
      (list
        (cons `foo "foo")
        (cons `bar "bar"))
      `(string-append foo bar))
    "foobar"))

(check
  (equal?
    (bindings-eval
      (list (cons `string+ string-append))
      `(string+ "foo" "bar"))
    "foobar"))
