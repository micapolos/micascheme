(import
  (typico core lookup)
  (typico library))

(check-library+syntax (core-lookup)
  (value x 10)
  (library
    (export
      (x integer))
    (define x 10)))

(check-library+syntax (core-lookup)
  (function (foo (integer x) (integer y)) (+ x y))
  (library
    (export
      (foo (function (integer integer) integer)))
    (define foo
      (lambda (x y)
        (($primitive 3 +) x y)))))
