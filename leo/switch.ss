(library (leo switch)
  (export switch)
  (import
    (rename (micascheme) (switch %switch))
    (keyword))

  (define-rules-syntaxes
    (keywords when else)

    (
      (switch (id . xs))
      (keyword? id)
      (switch id . xs))

    (
      (switch expr
        (when (var test?) x xs ...)
        ...
        (else (else-var else-x else-xs ...)))
      (%switch expr
        ((test? var) (run x xs ...))
        ...
        ((else else-var) (run else-x else-xs ...))))

    (
      (switch expr
        (when (var test?) x xs ...)
        ...)
      (%switch expr
        ((test? var) (run x xs ...))
        ...)))
)
