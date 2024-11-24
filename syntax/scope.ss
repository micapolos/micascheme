(library (syntax scope)
  (export
    empty-scope
    scope+
    scope+undefined
    scope-gen
    scope-ref
    scope-item
    scope-transformer
    scope-transform
    scope-unbound
    scope-gen?
    scope-with)
  (import (micascheme))

  (define scope-gen?
    (make-parameter #t))

  (define (empty-scope)
    (lambda (_) #f))

  (define (scope-ref $scope $id)
    ($scope $id))

  (define (scope-item $scope $id)
    (or
      (scope-ref $scope $id)
      (scope-unbound $id)))

  (define (scope+ $scope $id $item)
    (lambda ($lookup-id)
      (if (free-identifier=? $lookup-id $id)
        $item
        (scope-ref $scope $lookup-id))))

  (define (scope+undefined $scope $id $item)
    (if (scope-ref $scope $id)
      (syntax-error $id "already defined")
      (scope+ $scope $id $item)))

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

  (define-rule-syntax (scope-with (id item) ...)
    (fluent (empty-scope)
      (scope+ #'id #'item) ...))
)
