(library (leo set!)
  (export set!)
  (import
    (rename (scheme) (set! %set!))
    (identifier)
    (keyword))

  (define-syntax (set! $syntax)
    (syntax-case $syntax ()
      ((_ (target (id expr ...)))
        (keyword? id)
        #`(
          #,(identifier-append #'target #'target #'- #'id #'- #'set!)
          expr ...))
      ((_ (id expr))
        (keyword? id)
        #`(%set! id expr))))
)
