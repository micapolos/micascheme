(library (leo adjective)
  (export
    adjective
    adjective?
    adjective-keyword?
    define-adjective
    define-adjectives)
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

  (define-keyword adjective)

  (define-rule-syntax (adjective-keyword? lookup? k)
    (and
      (keyword k)
      (not-false? (safe-lookup? lookup? #'k #'adjective))))

  (define-syntax (adjective? stx)
    (lambda (lookup?)
      (syntax-case stx ()
        ((_ x)
          (literal->syntax
            (adjective-keyword? lookup? x))))))

  (define-rule-syntax (define-adjective x)
    (begin
      (define-keyword x)
      (define-property x adjective #t)))

  (define-rule-syntax (define-adjectives x ...)
    (begin (define-adjective x) ...))
)
