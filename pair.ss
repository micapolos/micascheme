(library (pair)
  (export
    pair-values
    null-or-pair?
    pair)
  (import
    (scheme)
    (binder))

  (define (pair-values $pair)
    (values (car $pair) (cdr $pair)))

  (define (null-or-pair? $obj)
    (or (null? $obj) (pair? $obj)))

  (define pair cons)

  (define-binder pair
    (lambda ($pair $fn)
      ($fn (car $pair) (cdr $pair))))
)
