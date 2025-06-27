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
    (typico core types)
    (asm u))

  (define-rule-syntax (check-expand-core in out)
    (check-expand core-expander in out))

  (define-rule-syntax (check-expand-core-raises in)
    (check-expand-raises core-expander in))

  (define-rule-syntax (vararg-op-expander id proc type)
    (case-expander (id x x* (... ...)) ($recurse)
      (lets
        ($proc proc)
        ($type type)
        ($typed-list (map (partial expand-inner $recurse) #'(x x* (... ...))))
        ($types (map typed-type $typed-list))
        (and
          (for-all (partial type=? $type) $types)
          (typed type
            (lets
              ($values (map typed-value $typed-list))
              ($value-predicate? (type-value-predicate? $type))
              ($value-datum-proc? (type-value-datum-proc? $type))
              (cond
                ((and
                  $value-predicate?
                  $value-datum-proc?
                  (for-all $value-predicate? $values))
                  ($value-datum-proc? (apply $proc $values)))
                (else `(proc ,@$values)))))))))

  (define core-expander
    (or-expander
      (predicate-expander boolean? boolean-type)
      (predicate-expander integer? integer-type)
      (predicate-expander char? char-type)
      (predicate-expander string? string-type)

      (case-expander (u8 x) ($recurse)
        (syntax-case (expand-inner-value $recurse integer-type #'x) ()
          (u8
            (u8? (datum u8))
            (typed u8-type (datum u8)))))

      (case-expander (if condition true false) ($recurse)
        (lets
          ($condition-value (expand-inner-value $recurse boolean-type #'condition))
          ((typed $type $true-value) (expand-inner $recurse #'true))
          ($false-value (expand-inner-value $recurse $type #'false))
          (typed $type
            (cond
              ((boolean? $condition-value) (if $condition-value $true-value $false-value))
              (else `(if ,$condition-value ,$true-value ,$false-value))))))

      (case-expander integer-zero (typed integer-type 0))
      (case-expander integer-one (typed integer-type 1))

      (vararg-op-expander + ($primitive 3 +) integer-type)
      (vararg-op-expander - ($primitive 3 +) integer-type)
      (vararg-op-expander + ($primitive 3 string-append) string-type)))
)
