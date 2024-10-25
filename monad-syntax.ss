(library (monad-syntax)
  (export
    define-pure-syntax
    define-pure
    pure

    define-bind-syntax
    define-bind
    bind)
  (import (scheme) (syntax) (syntaxes))

  (define-syntax (define-pure-syntax $syntax)
    (syntax-case $syntax ()
      ((_ id syntax)
        #`(define-property id pure syntax))))

  (define-syntax (define-pure $syntax)
    (syntax-case $syntax ()
      ((_ (id value) body)
        #`(define-pure id
          (lambda (value) body)))
      ((_ id fn)
        #`(begin
          (define id fn)
          (define-pure-syntax id
            (lambda ($syntax)
              (syntax-case $syntax ()
                ((_ body)
                  #`(id body )))))))))

  (define-lookup-syntax (pure $syntax $lookup)
    (syntax-case $syntax ()
      ((pure id body)
        (let ()
          (define $fn ($lookup #'id #'pure))
          (unless $fn (syntax-error $syntax "pure undefined"))
          ($fn #'(pure body))))))

  (define-syntax (define-bind-syntax $syntax)
    (syntax-case $syntax ()
      ((_ id syntax)
        #`(define-property id bind syntax))))

  (define-syntax (define-bind $syntax)
    (syntax-case $syntax ()
      ((_ (id value fn) body)
        #`(begin
          (define (bind-fn value fn) body)
          (define-bind-syntax id
            (lambda ($syntax)
              (syntax-case $syntax ()
                ((_ (var expr) body2)
                  #`(bind-fn expr (lambda (var) body2))))))))))

  (define-lookup-syntax (bind0 $syntax $lookup)
    (syntax-case $syntax ()
      ((_ id (val expr) body)
        (let ()
          (define $fn ($lookup #'id #'bind))
          (unless $fn (syntax-error $syntax "bind undefined"))
          ($fn #'(bind (val expr) body))))))

  (define-rules-syntax
    ((bind id body) body)
    ((bind id (var expr) rest ...)
      (bind0 id (var expr)
        (bind id rest ...))))
)
