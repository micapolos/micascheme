(library (leo switch)
  (export switch switch?)
  (import
    (scheme)
    (rename (switch)
      (switch %switch)
      (switch? %switch?))
    (keyword)
    (procedure)
    (syntaxes))

  (define-rules-syntaxes
    (keywords when else)

    ((switch (id . xs))
      (keyword? id)
      (switch id . xs))

    ((switch? (id . xs))
      (keyword? id)
      (switch? id . xs))

    ((switch expr (when (var test?) x xs ...) ... (else (else-var else-x else-xs ...)))
      (%switch expr ((test? var) (run x xs ...)) ... ((else else-var) (run else-x else-xs ...))))

    ((switch expr (when (var test?) x xs ...) ...)
      (%switch expr ((test? var) (run x xs ...)) ...))

    ((switch? expr (when (var test?) x xs ...) ...)
      (%switch? expr ((test? var) (run x xs ...)) ...)))
)
