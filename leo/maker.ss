(library (leo maker)
  (export maker)
  (import
    (scheme)
    (identifier))

  (define-syntax (maker $syntax)
    (lambda (lookup?)
      (syntax-case $syntax ()
        ((_ id)
          (guard
            (condition ((syntax-violation? condition) #f))
            (lookup? #'id #'maker))
          (lookup? #'id #'maker))
        ((_ id)
          (identifier-append #'id #'make #'- #'id)))))
)
