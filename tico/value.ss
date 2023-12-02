(library (tico value)
  (export
    value-arity
    value-application
    value-abstraction
    value-struct
    value-ref
    tuple-value
    tuple-ref-value)
  (import
    (micascheme)
    (evaluator)
    (tico arity)
    (tico expression)
    (tico datum))

  (function (value-environment)
    (environment '(micascheme) '(tico tuple)))

  (function (value-arity $value)
    (arity (length $value)))

  (function (value-application $target $args)
    (apply $target $args))

  (function (value-abstraction $arity $body)
    (lets
      ($body-symbol (generate-symbol))
      (evaluate
        (evaluator
          (value-environment)
          (list (cons $body-symbol $body)))
        (datum-abstraction
          (generate-symbols $arity)
          $body-symbol))))

  (function (value-struct $name $field-values)
    (tuple-value $field-values))

  (function (value-ref $arity $target $index)
    (tuple-ref-value $arity $target $index))

  (function (tuple-value $values)
    (lets
      ($symbols (generate-symbols (length $values)))
      (evaluate
        (evaluator
          (value-environment)
          (map cons $symbols $values))
        (datum-tuple $symbols))))

  (function (tuple-ref-value $arity $tuple $index)
    (lets
      ($symbol (generate-symbol))
      (evaluate
        (evaluator
          (value-environment)
          (stack
            (cons $symbol $tuple)))
        (datum-ref $arity $symbol $index))))
)
