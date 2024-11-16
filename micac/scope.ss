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
    scope-unique-gen?)
  (import (micascheme))

  (define scope-unique-gen?
    (make-parameter #f))

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
        (if (scope-unique-gen?)
          $id
          (parameterize
            ((gensym-prefix
              (fluent $id
                (syntax->datum)
                (symbol->string)
                (string-append "_"))))
            (fluent (list $id)
              (generate-temporaries)
              (car)
              (syntax->datum)
              (symbol->string)
              (string->symbol)
              (with $it (datum->syntax $id $it))))))
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
