(library (leo set!)
  (export set!)
  (import
    (rename (scheme) (set! %set!))
    (leo setter!)
    (keyword)
    (syntaxes))

  (define-rules-syntax
    ((set! (target (id expr ...)))
      (and (keyword? target) (keyword? id))
      ((setter! (target id)) expr ...))
    ((set! (id expr))
      (keyword? id)
      (%set! id expr)))
)
