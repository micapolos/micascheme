(library (pair)
  (export
    null-or-pair?
    pair)
  (import
    (scheme)
    (lets)
    (binder))

  (define (null-or-pair? $obj)
    (or (null? $obj) (pair? $obj)))

  (define pair cons)

  (define-bind pair
    (syntax-rules ()
      ((_ ($pair $car $cdr) $body)
        (lets
          ($car (car $pair))
          ($cdr (cdr $pair))
          $body))))
)
