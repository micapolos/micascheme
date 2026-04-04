(library (leo definer)
  (export definer)
  (import
    (except (scheme) syntax-error)
    (leo syntax-error)
    (syntax))

  (define-syntax (definer $syntax)
    (lambda (lookup?)
      (syntax-case $syntax ()
        ((_ (id . x))
          (lookup? #'id #'definer)
          (lookup? #'id #'definer)))))
)
