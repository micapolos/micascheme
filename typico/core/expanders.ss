(library (typico core expanders)
  (export
    core-expander
    check-expand-core
    check-expand-core-raises)
  (import
    (typico base)
    (typico expander)
    (typico typed)
    (typico type)
    (typico core types))

  (define-rule-syntax (check-expand-core in out)
    (check-expand core-expander in out))

  (define-rule-syntax (check-expand-core-raises in)
    (check-expand-raises core-expander in))

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
          (typed $type `(if ,$cond-value ,$true-value ,$false-value))))

      (case-expander integer-zero (typed integer-type 0))
      (case-expander integer-one (typed integer-type 1))

      (case-expander (+ x x* ...) ($recurse)
        (and
          (for-all (dot number? datum/annotation-stripped) #'(x x* ...))
          ($recurse (apply + (map datum/annotation-stripped #'(x x* ...))))))

      (case-expander (+ x x* ...) ($recurse)
        (lets
          ($typed-xs (map (partial expand-inner $recurse) #'(x x* ...)))
          (and
            (for-all (partial type=? integer-type) (map typed-type $typed-xs))
            (typed integer-type `(($primitive 3 +) ,@(map typed-value $typed-xs))))))))
)
