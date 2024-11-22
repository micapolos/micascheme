(library (micac scoped)
  (export
    scoped scoped? scoped-scope scoped-value
    scoped+
    scoped-ref
    scoped-gen
    scoped-transformer
    scoped-transform
    scoped-with
    scoped-map)
  (import
    (micascheme)
    (syntax scope))

  (data (scoped scope value))

  (define (scoped+ $scoped $id $item)
    (scoped
      (scope+ (scoped-scope $scoped) $id $item)
      (scoped-value $scoped)))

  (define (scoped-ref $scoped $id)
    (scope-ref (scoped-scope $scoped) $id))

  (define (scoped-gen $scope $id)
    (lets
      ((pair $scope $identifier) (scope-gen $scope $id))
      (scoped $scope $identifier)))

  (define (scoped-transformer $scoped $id)
    (scope-transformer (scoped-scope $scoped) $id))

  (define (scoped-transform $scoped $transformer $syntax)
    (scope-transform (scoped-scope $scoped) $transformer $syntax))

  (define-rule-syntax (scoped-with scoped-expr value)
    (scoped (scoped-scope scoped-expr) value))

  (define-rules-syntax
    ((scoped-map (value scoped-expr) body)
      (lets
        ($scoped scoped-expr)
        (value (scoped-value $scoped))
        (scoped (scoped-scope $scoped) body)))
    ((scoped-map decl decls ... body)
      (scoped-map decls ...
        (scoped-value (scoped-map decl body)))))
)
