(library (tico value)
  (export
    value-application
    value-struct
    value-ref
    tuple-value
    tuple-ref-value)
  (import
    (micascheme)
    (evaluator)
    (tico expression)
    (tico datum))

  (define (value-environment)
    (environment '(micascheme)))

  (define (value-application $target $args)
    (apply $target $args))

  (define (value-struct $name $field-values)
    (tuple-value $field-values))

  (define (value-ref $arity $target $index)
    (tuple-ref-value $arity $target $index))

  (define (tuple-value $values)
    (lets
      ($symbols (generate-symbols (length $values)))
      (evaluate
        (evaluator
          (value-environment)
          (map cons $symbols $values))
        (datum-tuple $symbols))))

  (define (tuple-ref-value $arity $tuple $index)
    (lets
      ($symbol (generate-symbol))
      (evaluate
        (evaluator
          (value-environment)
          (stack
            (cons $symbol $tuple)))
        (datum-ref $arity $symbol $index))))
)
