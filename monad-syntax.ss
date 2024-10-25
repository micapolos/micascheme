(library (monad-syntax)
  (export
    define-pure
    pure
    define-bind
    bind
    fmap)
  (import (scheme) (syntax) (syntaxes))

  (define-rules-syntax
    ((define-pure (id var) body)
      (define-pure id
        (lambda (var) body)))
    ((define-pure id fn)
      (begin
        (define var fn)
        (define-property id pure #'var))))

  (define-lookup-syntax (pure $syntax $lookup)
    (syntax-case $syntax ()
      ((pure id)
        (or
          ($lookup #'id #'pure)
          (syntax-error $syntax "undefined")))
      ((pure id value)
        #`((pure id) value))))

  (define-rules-syntax
    ((define-bind (id fn value) body)
      (and (identifier? #'id) (identifier? #'fn) (identifier? #'value))
      (define-bind id
        (lambda (fn value) body)))
    ((define-bind id fn)
      (identifier? #'id)
      (begin
        (define var fn)
        (define-property id bind #'var))))

  (define-lookup-syntax (bind $syntax $lookup)
    (syntax-case $syntax ()
      ((bind id)
        (or
          ($lookup #'id #'bind)
          (syntax-error $syntax "undefined")))
      ((bind id fn value)
        #`((bind id) fn value))))

  (define-rules-syntax
    ((fmap id)
      (lambda (fn value)
        (bind id
          (lambda (x) (pure id (fn x)))
          value)))
    ((fmap id fn value)
      ((fmap id) fn value)))
)
