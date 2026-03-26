(library (pair)
  (export
    null/pair?
    singleton-list?
    pair
    pair-map
    map-car
    map-cdr
    with-car
    with-cdr
    cons/identity)
  (import
    (scheme)
    (lets)
    (binder))

  (define (null/pair? $obj)
    (or (null? $obj) (pair? $obj)))

  (define (singleton-list? $obj)
    (and (pair? $obj) (null? (cdr $obj))))

  (define pair cons)

  (define-bind pair
    (syntax-rules ()
      ((_ ($pair $car $cdr) $body)
        (lets
          ($car (car $pair))
          ($cdr (cdr $pair))
          $body))))

  (define (pair-map $fn $pair)
    (cons
      ($fn (car $pair))
      ($fn (cdr $pair))))

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

  (define (cons/identity $car $cdr)
    (if (null? $cdr) $car (cons $car $cdr)))
)
