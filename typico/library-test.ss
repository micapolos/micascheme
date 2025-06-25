(import
  (typico core lookup)
  (typico library))

(check-library+syntax
  (core-lookup)
  (value x 10)
  (library (define x 10))
  (x integer))

(check-library+syntax
  (core-lookup)
  (function (foo (integer x) (integer y)) (+ x y))
  (library
    (define foo
      (lambda (x y)
        (($primitive 3 +) x y))))
  (foo (function (integer integer) integer)))
