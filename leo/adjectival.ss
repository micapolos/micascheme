(library (leo adjectival)
  (export transform-adjectival)
  (import
    (scheme)
    (syntax)
    (keyword)
    (identifier)
    (system)
    (lets)
    (leo adjective))

  (define (transform-adjectival identifier-adjective? stx)
    (or
      (syntax-case? stx ()
        ((id x)
          (and
            (keyword? id)
            (identifier-adjective? #'id))
          (transform-prefixed-adjectival identifier-adjective? #'id stx)))
      stx))

  (define (transform-prefixed-adjectival identifier-adjective? tpl stx)
    (or
      (syntax-case? stx ()
        ((prefix x)
          (keyword? x)
          (identifier-append tpl #'prefix #'- #'x))
        ((prefix (id x))
          (keyword? id)
          (lets
            (stx #`(#,(identifier-append tpl #'prefix #'- #'id) x))
            (if (identifier-adjective? #'id)
              (transform-prefixed-adjectival identifier-adjective? tpl stx)
              stx))))
      stx))
)
