(library
  (leo define-property)
  (export define-property)
  (import
    (rename (scheme)
      (define-property %define-property))
    (syntax))

  (define-rule-syntax (define-property (id (key value)))
    (%define-property id key value))
)
