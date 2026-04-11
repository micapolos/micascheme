(library (leo set)
  (export set!)
  (import
    (rename (scheme) (set! %set!))
    (syntax))

  (define-rule-syntax (set! (x expr))
    (%set! x expr))
)
