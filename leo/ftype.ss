(library
  (leo ftype)
  (export
    define-ftype
    ftype-ref
    ftype*-ref
    ftype-&ref
    ftype*-&ref
    ftype-any-ref
    ftype*-any-ref
    ftype-set!
    ftype*-set!
    ftype-any-set!
    ftype*-any-set!)
  (import
    (rename
      (except (scheme) syntax-error)
      (define-ftype %define-ftype)
      (ftype-ref %ftype-ref)
      (ftype-&ref %ftype-&ref)
      (ftype-any-ref %ftype-any-ref)
      (ftype-set! %ftype-set!)
      (ftype-any-set! %ftype-any-set!))
    (keyword)
    (syntax)
    (syntaxes)
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

  (define-rules-syntaxes
    ((ftype-ref ftype-name x a ...)
      (%ftype-ref ftype-name (a ...) x))
    ((ftype*-ref ftype-name x index a ...)
      (%ftype-ref ftype-name (a ...) x index))
    ((ftype-&ref ftype-name x a ...)
      (%ftype-&ref ftype-name (a ...) x))
    ((ftype*-&ref ftype-name x index a ...)
      (%ftype-&ref ftype-name (a ...) x index))
    ((ftype-any-ref ftype-name x a ...)
      (%ftype-any-ref ftype-name (a ...) x))
    ((ftype*-any-ref ftype-name x offset a ...)
      (%ftype-any-ref ftype-name (a ...) x offset)))

  (define-rules-syntaxes
    ((ftype-set! ftype-name x a ... expr)
      (%ftype-set! ftype-name (a ...) x expr))
    ((ftype*-set! ftype-name x index a ... expr)
      (%ftype-set! ftype-name (a ...) x index expr))
    ((ftype-any-set! ftype-name x a ... expr)
      (%ftype-any-set! ftype-name (a ...) x expr))
    ((ftype*-any-set! ftype-name x offset a ... expr)
      (%ftype-any-set! ftype-name (a ...) x offset expr)))
)
