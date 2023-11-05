(library (tico value)
  (export
    value-application
    value-struct
    tuple-value
    tuple-ref-value)
  (import
    (micascheme)
    (evaluator)
    (tico expression))

  (define (value-environment)
    (environment '(micascheme)))

  (define (value-application $target $args)
    (apply $target $args))

  (define (value-struct $name $field-values)
    (tuple-value $field-values))

  (define (tuple-value $values)
    (lets
      ($symbols (generate-symbols (length $values)))
      (evaluate
        (evaluator
          (value-environment)
          (map cons $symbols $values))
        (tuple-expression $symbols))))

  (define (tuple-ref-value $arity $tuple $index)
    (lets
      ($symbol (generate-symbol))
      (evaluate
        (evaluator
          (value-environment)
          (stack
            (cons $symbol $tuple)))
        (tuple-ref-expression $arity $symbol $index))))
)
