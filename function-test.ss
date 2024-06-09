(import (scheme) (check) (function) (binder) (procedure) (lets))

(run
  (function (plus a b) (string-append a b))
  (check (equal? (plus "foo" "bar") "foobar")))

(run
  (define-bind cons
    (syntax-rules ()
      ((_ ($id $car $cdr) $body)
        (lets
          ($car (car $id))
          ($cdr (cdr $id))
          $body))))

  (run
    (function (plus (cons a b) (cons c d))
      (string-append a b c d))

    (check
      (equal?
        (plus (cons "foo" "bar") (cons "!" "?"))
        "foobar!?")))

  (run
    (function (plus-tail a b . c)
      (string-append a b (apply string-append c)))

    (check
      (equal?
        (plus-tail "foo" "bar")
        "foobar"))

    (check
      (equal?
        (plus-tail "foo" "bar" "!" "?")
        "foobar!?")))

  (run
    (function (plus (cons (cons a b) (cons c d)))
      (string-append a b c d))

    (check
      (equal?
        (plus (cons (cons "foo" "bar") (cons "!" "?")))
        "foobar!?"))))


