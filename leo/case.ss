(library (leo case)
  (export case)
  (import
    (rename (scheme) (case %case))
    (syntaxes))

  (define-rules-syntax
    (keywords when else values)

    ((case expr (when (values v ...) x xs ...) ... (else e es ...))
      (%case expr ((v ...) x xs ...) ... (else e es ...)))

    ((case expr (when v x xs ...) ... (else e es ...))
      (%case expr (v x xs ...) ... (else e es ...)))

    ((case expr (when (values v ...) x xs ...) ...)
      (%case expr ((v ...) x xs ...) ...))

    ((case expr (when v x xs ...) ...)
      (%case expr (v x xs ...) ...)))
)
