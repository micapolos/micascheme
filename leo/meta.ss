(library (leo meta)
  (export meta)
  (import
    (rename (scheme) (meta %meta))
    (syntax))

  (define-rule-syntax (meta x) (%meta . x))
)
