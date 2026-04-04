(library (leo predicate)
  (export predicate)
  (import
    (except (scheme) predicate)
    (identifier))

  (define-syntax (predicate $syntax)
    (lambda (lookup?)
      (syntax-case $syntax ()
        ((_ id)
          (guard
            (condition ((syntax-violation? condition) #f))
            (lookup? #'id #'predicate))
          (lookup? #'id #'predicate))
        ((_ id)
          (identifier-append #'id #'id #'?)))))
)
