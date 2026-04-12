(library (leo identifier-syntax)
  (export identifier-syntax)
  (import
    (rename (scheme)
      (identifier-syntax %identifier-syntax)
      (set! %set!))
    (syntaxes)
    (keyword)
    (leo with)
    (leo set))

  (define-rules-syntax
    ((identifier-syntax getter)
      (%identifier-syntax getter))
    ((identifier-syntax getter (v setter))
      (%identifier-syntax (id getter) ((%set! id v) setter))))
)
