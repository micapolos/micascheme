(library (pair)
  (export
    null-or-pair?
    pair
    map-car
    map-cdr
    with-car
    with-cdr)
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

  (define (map-car $proc $pair)
    (cons ($proc (car $pair)) (cdr $pair)))

  (define (map-cdr $proc $pair)
    (cons (car $pair) ($proc (cdr $pair))))

  (define-syntax with-car
    (syntax-rules ()
      ((_ (id pair) body)
        (map-car (lambda (id) body) pair))))

  (define-syntax with-cdr
    (syntax-rules ()
      ((_ (id pair) body)
        (map-cdr (lambda (id) body) pair))))
)
