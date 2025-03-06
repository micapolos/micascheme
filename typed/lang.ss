(library (typed lang)
  (export
    : assume
    tt
    typeof type
    define-type
    define-typed)
  (import
    (micascheme)
    (typed type)
    (typed typed)
    (typed syntax)
    (typed keywords))

  (meta define (type-lookup $lookup)
    (lambda ($identifier)
      ($lookup $identifier #'type)))

  (define-syntax (tt $syntax $lookup)
    (syntax-case $syntax ()
      ((_ expr)
        (typed-value
          (syntax->typed
            (lambda ($type-syntax)
              (eval
                (syntax->datum $type-syntax)
                (environment '(micascheme) '(typed type))))
            (type-lookup $lookup)
            #'expr)))))

  (define-syntax (define-typed $syntax $lookup)
    (syntax-case $syntax ()
      ((_ id expr)
        (identifier? #'id)
        (lets
          ($typed
            (syntax->typed
              (lambda ($type-syntax)
                (eval
                  (syntax->datum $type-syntax)
                  (environment '(micascheme) '(typed type))))
              (type-lookup $lookup)
              #'expr))
          #`(begin
            (define id #,(typed-value $typed))
            (define-property id type #,(type->syntax (typed-type $typed))))))))
)
