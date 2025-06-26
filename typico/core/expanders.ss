(library (typico core expanders)
  (export
    core-expander
    check-expand-core)
  (import
    (typico base)
    (typico expander)
    (typico typed)
    (typico core types))

  (define core-expander
    (or-expander
      (predicate-expander boolean? boolean-type)
      (predicate-expander integer? integer-type)
      (predicate-expander char? char-type)
      (predicate-expander string? string-type)

      (case-expander (if cond true false) ($recurse)
        (lets
          ($cond-value (expand-inner-value $recurse boolean-type #'cond))
          ((typed $type $true-value) (expand-inner $recurse #'true))
          ($false-value (expand-inner-value $recurse $type #'false))
          (typed $type `(if ,$cond-value ,$true-value ,$false-value))))))

  (define-rule-syntax (check-expand-core in out)
    (check-expand core-expander in out))
)
