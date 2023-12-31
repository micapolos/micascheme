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

  (define (value-environment)
    (environment '(micascheme) '(tico tuple)))

  (define (value-arity $value)
    (arity (length $value)))

  (define (value-application $target $args)
    (apply $target $args))

  (define (value-abstraction $arity $body)
    (lets
      ($body-symbol (generate-symbol))
      (evaluate
        (evaluator
          (value-environment)
          (list (cons $body-symbol $body)))
        (datum-abstraction
          (generate-symbols $arity)
          $body-symbol))))

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
