(library (leo ref)
  (export ref)
  (import
    (scheme)
    (identifier))

  (define-syntax (ref $syntax)
    (syntax-case $syntax ()
      ((_ (target (field x ...)))
        #`(
          #,(identifier-append #'target #'target #'- #'field #'- #'ref)
          x ...))))
)
