(library (tico value)
  (export
    tuple-value)
  (import
    (micascheme)
    (evaluator)
    (tico expression))

  (define (tuple-value $values)
    (lets
      ($symbols (generate-symbols (length $values)))
      (evaluate
        (evaluator
          (environment `(micascheme) `(tico expression))
          (map cons $symbols $values))
        (tuple-expression $symbols))))
)
