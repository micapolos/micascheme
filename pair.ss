(library (pair)
  (export
    null-or-pair?
    pair)
  (import
    (scheme)
    (binder))

  (define (null-or-pair? $obj)
    (or (null? $obj) (pair? $obj)))

  (define pair cons)

  (define-binder pair
    (lambda ($pair $fn)
      ($fn (car $pair) (cdr $pair))))
)
