(library (leo case)
  (export case)
  (import
    (rename (scheme) (case %case))
    (syntaxes)
    (keyword))

  (define-rules-syntax
    (keywords when else values)

    ((case (id . x))
      (keyword? id)
      (case id . x))

    ((case expr (when (values v ...) x xs ...) ... (else e es ...))
      (%case expr ((v ...) x xs ...) ... (else e es ...)))

    ((case expr (when v x xs ...) ... (else e es ...))
      (%case expr (v x xs ...) ... (else e es ...)))

    ((case expr (when (values v ...) x xs ...) ...)
      (%case expr ((v ...) x xs ...) ...))

    ((case expr (when v x xs ...) ...)
      (%case expr (v x xs ...) ...)))
)
