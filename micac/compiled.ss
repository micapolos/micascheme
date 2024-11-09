(library (micac compiled)
  (export
    compiled compiled? compiled-env compiled-value
    compiled-ref
    compiled-transform
    pure-compiled
    compiled-with
    compiled-map)
  (import
    (micascheme)
    (micac env))

  (data (compiled env value))

  (define (compiled-ref $compiled $id)
    (env-ref (compiled-env $compiled) $id))

  (define (compiled-transform $compiled $id $syntax)
    (env-transform (compiled-env $compiled) $id $syntax))

  (define-rule-syntax (pure-compiled value)
    (compiled empty-env value))

  (define-rule-syntax (compiled-with compiled-expr value)
    (compiled (compiled-env compiled-expr) value))

  (define-rules-syntax
    ((compiled-map body)
      (pure-compiled body))
    ((compiled-map (value compiled-expr) body)
      (lets
        ($compiled compiled-expr)
        (value (compiled-value $compiled))
        (compiled (compiled-env $compiled) body)))
    ((compiled-map decl decls ... body)
      (compiled-map decls ...
        (compiled-value (compiled-map decl body)))))
)
