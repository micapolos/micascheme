(library (leo is?)
  (export is?)
  (import
    (scheme)
    (identifier))

  (define-syntax (is? $syntax)
    (syntax-case $syntax ()
      ((_ (id expr ...))
        #`(
          #,(identifier-append #'id #'id #'?)
          expr ...))))
)
