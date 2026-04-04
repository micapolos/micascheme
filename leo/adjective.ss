(library (leo adjective)
  (export
    adjective
    adjectives
    adjective?
    identifier-adjective?)
  (import
    (except (scheme) syntax-error)
    (syntax)
    (data)
    (boolean)
    (syntaxes)
    (keyword)
    (identifier)
    (system)
    (leo lookup)
    (leo syntax-error))

  (define-keywords adjective adjectives)

  (meta define (identifier-adjective? lookup? identifier)
    (not-false? (safe-lookup? lookup? identifier #'adjective)))

  (define-syntax (adjective? stx)
    (lambda (lookup?)
      (syntax-case stx ()
        ((_ x)
          (literal->syntax
            (and
              (keyword? x)
              (identifier-adjective? lookup? #'x)))))))
)
