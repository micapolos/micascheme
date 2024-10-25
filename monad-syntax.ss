(library (monad-syntax)
  (export
    define-pure
    pure
    define-bind
    bind
    monad-define
    fmap
    flat-map)
  (import (scheme) (syntax) (syntaxes))

  (define-rules-syntax
    ((define-pure id value)
      (identifier? #'id)
      (begin
        (define var value)
        (define-property id pure #'var)))
    ((define-pure (id value) body)
      (and (identifier? #'id) (identifier? #'value))
      (define-pure id
        (lambda (value) body))))

  (define-lookup-syntax (pure $syntax $lookup)
    (syntax-case $syntax ()
      ((pure id)
        (identifier? #'id)
        (or
          ($lookup #'id #'pure)
          (syntax-error $syntax "undefined")))
      ((pure (id value))
        (identifier? #'id)
        #`((pure id) value))))

  (define-rules-syntax
    ((define-bind id fn)
      (identifier? #'id)
      (begin
        (define var fn)
        (define-property id bind #'var)))
    ((define-bind (id fn monad) body)
      (and (identifier? #'id) (identifier? #'fn) (identifier? #'monad))
      (define-bind id
        (lambda (fn monad) body))))

  (define-lookup-syntax (bind $syntax $lookup)
    (syntax-case $syntax ()
      ((bind id)
        (identifier? #'id)
        (or
          ($lookup #'id #'bind)
          (syntax-error $syntax "undefined")))
      ((bind id fn monad)
        (identifier? #'id)
        #`((bind id) fn monad))
      ((bind (id value monad) body)
        (and (identifier? #'id) (identifier? #'value))
        #`(bind id (lambda (value) body) monad))))

  (define-rules-syntax
    ((monad-define (name id arg ...) body)
      (define-rules-syntax
        ((name id)
          (identifier? #'id)
          (lambda (arg ...) body))
        ((name id arg ...)
          (identifier? #'id)
          ((name id) arg ...)))))

  (define-rules-syntax
    ((fmap id)
      (identifier? #'id)
      (lambda (fn monad)
        (bind (id value monad)
          (pure (id (fn value))))))
    ((fmap id fn monad)
      (identifier? #'id)
      ((fmap id) fn monad))
    ((fmap (id value monad) body)
      (and (identifier? #'id) (identifier? #'value))
      (fmap id (lambda (value) body) monad)))

  (monad-define (flat-map id monads)
    (fold-right
      (lambda (value-monad values-monad)
        (bind (id value value-monad)
          (bind (id values values-monad)
            (pure (id (cons value values))))))
      (pure (id (list)))
      monads))
)
