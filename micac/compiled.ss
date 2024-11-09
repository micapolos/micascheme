(library (micac compiled)
  (export
    compiled compiled? compiled-env compiled-value
    compiled-alloc
    compiled+
    compiled-ref
    compiled-transform
    compiled-with
    compiled-map)
  (import
    (micascheme)
    (micac env))

  (data (compiled env value))

  (define (compiled-alloc $compiled $id)
    (compiled
      (env-alloc (compiled-env $compiled) $id)
      (compiled-value $compiled)))

  (define (compiled+ $compiled $id $transformer)
    (compiled
      (env+ (compiled-env $compiled) $id $transformer)
      (compiled-value $compiled)))

  (define (compiled-ref $compiled $id)
    (env-ref (compiled-env $compiled) $id))

  (define (compiled-transform $compiled $id $syntax)
    (env-transform (compiled-env $compiled) $id $syntax))

  (define-rule-syntax (compiled-with compiled-expr value)
    (compiled (compiled-env compiled-expr) value))

  (define-rules-syntax
    ((compiled-map (value compiled-expr) body)
      (lets
        ($compiled compiled-expr)
        (value (compiled-value $compiled))
        (compiled (compiled-env $compiled) body)))
    ((compiled-map decl decls ... body)
      (compiled-map decls ...
        (compiled-value (compiled-map decl body)))))
)
