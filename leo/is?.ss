(library (leo is?)
  (export is?)
  (import
    (scheme)
    (leo is?-tester)
    (syntax))

  (define-rule-syntax (is? (id x))
    ((is?-tester id) x))
)
