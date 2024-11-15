(library (micac scope)
  (export
    scope scope? scope-lookup scope-size
    empty-scope
    scope+
    scope-gen
    scope-ref
    scope-transformer
    scope-unbound
    scope-with
    pretty-identifier?)
  (import
    (micascheme)
    (micac expr))

  (define pretty-identifier? (make-parameter #f))

  (data (scope lookup size))

  (define (empty-scope)
    (scope (lambda (_) #f) 0))

  (define (scope-ref $scope $id)
    (app (scope-lookup $scope) $id))

  (define (scope+ $scope $id $item)
    (scope
      (lambda ($lookup-id)
        (if (free-identifier=? $lookup-id $id)
          $item
          (scope-ref $scope $lookup-id)))
      (+ (scope-size $scope) 1)))

  (define (scope-with . $bindings)
    (fold-left scope+ (empty-scope) $bindings))

  (define (scope-gen $scope $id)
    (lets
      ($identifier
        (if (pretty-identifier?)
          $id
          (datum->syntax $id
            (string->symbol
              (string-append
                "v"
                (number->string (scope-size $scope))
                "-"
                (symbol->string (syntax->datum $id)))))))
      (pair
        (scope+ $scope $id $identifier)
        $identifier)))

  (define (scope-transformer $scope $id)
    (switch (scope-ref $scope $id)
      ((identifier? _) #f)
      ((false? _) #f)
      ((else $transformer) $transformer)))

  (define (scope-unbound $id)
    (syntax-error $id "unbound identifier"))
)
