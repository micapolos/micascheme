(library (typico expander)
  (export
    expander
    predicate-expander
    or-expander
    case-expander
    expand-typed
    expand-inner
    expand-inner-value
    check-expand
    check-expand-raises)
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

  (define-rules-syntax
    ((case-expander (id param ...) ($recurse) body ...)
      (expander ($recurse $syntax)
        (syntax-case? $syntax (id)
          ((id param ...) body ...)))))

  (define (expand-typed $expander $syntax)
    (or
      ($expander (partial expand-typed $expander) $syntax)
      (syntax-error $syntax)))

  (define (expand-inner $recurse $syntax)
    (or
      ($recurse $syntax)
      (syntax-error $syntax)))

  (define (expand-inner-value $recurse $expected-type $syntax)
    (lets
      ((typed $type $value) (expand-inner $recurse $syntax))
      (cond
        ((type=? $type $expected-type) $value)
        (else (type-error $syntax $type $expected-type)))))

  (define-rule-syntax (check-expand expander in out)
    (check
      (equal?
        (typed->datum (expand-typed expander (datum/annotation in)))
        'out)))

  (define-rule-syntax (check-expand-raises expander in)
    (check
      (raises
        (expand-typed expander (datum/annotation in)))))
)
