(library (micac compiled)
  (export
    compiled compiled? compiled-scope compiled-value
    compiled-ref
    compiled-transform
    pure-compiled
    compiled-with
    compiled-map)
  (import
    (micascheme)
    (micac scope))

  (data (compiled scope value))

  (define (lookup-scope-ref $lookup $scope $id)
    (or
      (scope-ref $scope $id)
      ($lookup $id)))

  (define (compiled-ref $lookup $compiled $id)
    (lookup-scope-ref $lookup (compiled-scope $compiled) $id))

  (define (lookup-scope-transform $lookup $scope $id $syntax)
    (lets
      ($transformer (lookup-scope-ref $lookup $scope $id))
      (if $transformer
        (transform $transformer $syntax $lookup)
        (syntax-error $id "no macro"))))

  (define (compiled-transform $lookup $compiled $id $syntax)
    (lets
      ($transformer (compiled-ref $lookup $compiled $id))
      (if $transformer
        (transform $transformer $syntax $lookup)
        (syntax-error $id "no macro"))))

  (define-rule-syntax (pure-compiled value)
    (compiled (scope) value))

  (define-rule-syntax (compiled-with compiled-expr value)
    (compiled (compiled-scope compiled-expr) value))

  (define-rules-syntax
    ((compiled-map body)
      (pure-compiled body))
    ((compiled-map (value compiled-expr) body)
      (lets
        ($compiled compiled-expr)
        (value (compiled-value $compiled))
        (compiled (compiled-scope $compiled) body)))
    ((compiled-map decl decls ... body)
      (compiled-map decls ...
        (compiled-value (compiled-map decl body)))))

)
