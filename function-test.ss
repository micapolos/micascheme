(import (check) (function) (binder) (procedure))

(run
  (function (plus a b) (string-append a b))
  (check (equal? (plus "foo" "bar") "foobar")))

(run
  (define-binder cons
    (lambda ($pair $fn)
      ($fn (car $pair) (cdr $pair))))

  (function (plus (cons a b) (cons c d))
    (string-append a b c d))

  (function (plus-tail a b . c)
    (string-append a b (apply string-append c)))

  (check
    (equal?
      (plus (cons "foo" "bar") (cons "!" "?"))
      "foobar!?"))

  (check
    (equal?
      (plus-tail "foo" "bar")
      "foobar"))

  (check
    (equal?
      (plus-tail "foo" "bar" "!" "?")
      "foobar!?")))
