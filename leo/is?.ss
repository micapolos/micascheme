(library (leo is?)
  (export is?)
  (import
    (except (scheme) predicate)
    (leo predicate)
    (syntax))

  (define-rule-syntax (is? (id x))
    ((predicate id) x))
)
