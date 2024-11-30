(library (syntax scoped)
  (export
    scoped scoped? scoped-lookup scoped-value
    scoped+
    scoped-ref
    scoped-gen
    scoped-transformer
    scoped-transform
    scoped-with
    scoped-map)
  (import
    (micascheme)
    (syntax lookup))

  (data (scoped lookup value))

  (define (scoped+ $scoped $id $item)
    (scoped
      (lookup+ (scoped-lookup $scoped) $id $item)
      (scoped-value $scoped)))

  (define (scoped-ref $scoped $id)
    ((scoped-lookup $scoped) $id))

  (define (scoped-gen $scope $id)
    (lets
      ((pair $scope $identifier) (lookup-gen $scope $id))
      (scoped $scope $identifier)))

  (define (scoped-transformer $scoped $id)
    (lookup-transformer (scoped-lookup $scoped) $id))

  (define (scoped-transform $scoped $transformer $syntax)
    (lookup-transform (scoped-lookup $scoped) $transformer $syntax))

  (define-rule-syntax (scoped-with scoped-expr value)
    (scoped (scoped-lookup scoped-expr) value))

  (define-rules-syntax
    ((scoped-map (value scoped-expr) body)
      (lets
        ($scoped scoped-expr)
        (value (scoped-value $scoped))
        (scoped (scoped-lookup $scoped) body)))
    ((scoped-map decl decls ... body)
      (scoped-map decls ...
        (scoped-value (scoped-map decl body)))))
)
