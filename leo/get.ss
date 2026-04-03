(library (leo get)
  (export get)
  (import
    (scheme)
    (identifier))

  (define-syntax (get $syntax)
    (syntax-case $syntax ()
      ((_ (target (id expr ...)))
        #`(
          #,(identifier-append #'target #'target #'- #'id)
          expr ...))))
)
