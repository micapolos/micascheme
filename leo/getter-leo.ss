(library (leo getter-leo)
  (export getter)
  (import
    (scheme)
    (identifier))

  (define-syntax (getter $syntax)
    (syntax-case $syntax ()
      ((_ (target id))
        (identifier-append #'target #'target #'- #'id))))
)
