(library (micac scope)
  (export
    empty-scope
    scope+
    scope-gen
    scope-ref
    scope-transformer
    scope-transform
    scope-unbound
    scope-with
    scope-gen?)
  (import (micascheme))

  (define scope-gen?
    (make-parameter #t))

  (define (empty-scope)
    (lambda (_) #f))

  (define (scope-ref $scope $id)
    ($scope $id))

  (define (scope+ $scope $id $item)
    (lambda ($lookup-id)
      (if (free-identifier=? $lookup-id $id)
        $item
        (scope-ref $scope $lookup-id))))

  (define (scope-with . $bindings)
    (fold-left scope+ (empty-scope) $bindings))

  (define (scope-gen $scope $id)
    (lets
      ($identifier
        (if (scope-gen?)
          (generate-identifier $id)
          $id))
      (pair
        (scope+ $scope $id $identifier)
        $identifier)))

  (define (scope-transformer $scope $id)
    (switch (scope-ref $scope $id)
      ((identifier? _) #f)
      ((false? _) #f)
      ((else $transformer) $transformer)))

  (define (scope-transform $scope $transformer $syntax)
    (transform $transformer $syntax $scope))

  (define (scope-unbound $id)
    (syntax-error $id "unbound identifier"))
)
