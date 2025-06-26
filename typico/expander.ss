(library (typico expander)
  (export
    expander
    predicate-expander
    or-expander
    case-expander
    expand
    recurse-value
    check-expand)
  (import
    (typico base)
    (typico type)
    (typico typed)
    (only (typico expand) type-error))

  (define-rule-syntax (expander ($recurse $syntax) body)
    (lambda ($recurse $syntax) body))

  (define-rule-syntax (predicate-expander test? type)
    (expander ($recurse $syntax)
      (syntax-case? $syntax ()
        (x
          (test? (datum x))
          (typed type (datum x))))))

  (define-rule-syntax (or-expander $expander ...)
    (expander ($recurse $syntax)
      (or ($expander $recurse $syntax) ...)))

  (define-rule-syntax (case-expander (id param ...) ($recurse) body)
    (expander ($recurse $syntax)
      (syntax-case? $syntax (id)
        ((id param ...) body))))

  (define (expand $expander $syntax)
    (or
      ($expander (partial expand $expander) $syntax)
      (syntax-error $syntax)))

  (define (recurse-value $recurse $expected-type $syntax)
    (lets
      ((typed $type $value) ($recurse $syntax))
      (cond
        ((type=? $type $expected-type) $value)
        (else (type-error $syntax $type $expected-type)))))

  (define-rule-syntax (check-expand expander in out)
    (check
      (equal?
        (typed->datum (expand expander (datum/annotation in)))
        'out)))
)
