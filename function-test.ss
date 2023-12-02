(import (check) (function) (binder) (procedure))

(run
  (function (plus a b) (string-append a b))
  (check (equal? (plus "foo" "bar") "foobar")))

(run
  (define-binder cons
    (lambda ($pair $fn)
      ($fn (car $pair) (cdr $pair))))

  (function (plus (cons a b) c)
    (string-append a b c))

  (check
    (equal?
      (plus (cons "foo" "bar") "!")
      "foobar!")))
