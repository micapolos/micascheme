(library
  (leo define-ftype)
  (export define-ftype)
  (import
    (rename
      (except (scheme) syntax-error)
      (define-ftype %define-ftype))
    (keyword)
    (syntax)
    (leo syntax-error)
    (leo transform-ftype))

  (define-syntax (define-ftype stx)
    (syntax-case stx ()
      ((_ (name ftype))
        (keyword? name)
        #`(%define-ftype name
          #,(transform-ftype #'ftype)))
      ((_ xs ...)
        #`(begin (define-ftype xs) ...))
      (_
        (syntax-error stx
          '(expected (define-ftype (name ftype) ...))))))
)
