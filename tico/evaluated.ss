(library (tico evaluated)
  (export
    evaluated evaluated? evaluated-type evaluated-value)
  (import
    (micascheme)
    (tico type))

  (define (evaluated type value))

  (define (evaluated-unwrap $evaluated)
    (value-type-unwrap
      (evaluated-type $evaluated)
      (evaluated-value $evaluated)))

  (define (type-value-unwrap $type $value)
    (switch $type
      ((static? $static) (static-value $static))
      ((anything? _) $value)
      ((any-boolean? _) $value)
      ((any-number? _) $value)
      ((any-string? _) $value)
      ((any-list? $any-list)
        (lets
          ($type (any-list-item $any-list))
          (cond
            ((type-static? $type)
              (make-list
                (length $value)
                (type-value-unwrap $type #f)))
            (else
              (map
                (partial type-value-unwrap $type)
                $value)))))
      ((struct? $struct)
        (lets
          ($types (struct-items $struct))
          ($dynamic-types (filter type-dynamic? $types))
          ($arity (length $dynamic-types))
          ($values (value-arity->values $value $arity))
          (types-values-unwrap $types $values)))
      ((else $other)
        (throw `unwrap $type $value))))

  (define (value-arity->values $value $arity)
    (case $arity
      ((0) (list))
      ((1) (list $value))
      ((2) (list (car $value) (cdr $value)))
      (else (vector->list $value))))

  (define (values-types-unwrap $values $types)
    (reverse (unwrapped-push-values-types (stack) $values $types)))

  (define (unwrapped-push-values-types $unwrapped $values $types)
    (cond
      ((null? $types) $unwrapped)
      (else
        (unpair $types $type $types
          (cond
            ((type-static? $type)
              (unwrapped-push-values-types
                (push $unwrapped (value-type-unwrap #f $type))
                $values $types))
            (else
              (unwrapped-push-values-types
                (push $unwrapped (value-type-unwrap (car $values) $type))
                (cdr $values) $types)))))))
)
