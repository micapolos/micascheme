(library (leo maker)
  (export maker)
  (import
    (scheme)
    (identifier))

  (define-syntax (maker $syntax)
    (syntax-case $syntax ()
      ((_ id)
        (identifier-append #'id #'make #'- #'id))))
)
