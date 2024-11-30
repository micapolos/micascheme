(library (micalog core scope)
  (export
    scope scope? scope-renamed-id-lookup scope-id-value-lookup
    empty-scope
    value-lookup->scope
    scope+
    scope+gen
    scope+renamed
    scope-ref
    scope-commit)
  (import
    (micascheme)
    (prefix (syntax scope) syntax-))

  (data (scope renamed-id-lookup id-value-lookup))

  (define (value-lookup->id-value-lookup $value-lookup)
    (lambda ($id)
      (fluent
        ($value-lookup $id)
        (let $value? (and $value? (pair $id $value?))))))

  (define (value-lookup->scope $value-lookup)
    (fluent $value-lookup
      (value-lookup->id-value-lookup)
      (id-value-lookup->scope)))

  (define (id-value-lookup->scope $id-value-lookup)
    (scope (syntax-empty-scope) $id-value-lookup))

  (define (empty-scope)
    (id-value-lookup->scope (syntax-empty-scope)))

  (define (scope+renamed (scope $renamed-id-lookup $id-value-lookup) $id $renamed-id)
    (scope
      (syntax-scope+undefined
        $renamed-id-lookup
        (syntax-scope-undefined-id $id-value-lookup $id)
        $renamed-id)
      $id-value-lookup))

  (define (scope+ (scope $renamed-id-lookup $id-value-lookup) $id $value)
    (scope
      $renamed-id-lookup
      (syntax-scope+undefined
        $id-value-lookup
        (syntax-scope-undefined-id $renamed-id-lookup $id)
        (pair $id $value))))

  (define (scope+gen (scope $renamed-id-lookup $id-value-lookup) $id $value)
    (lets
      ($renamed-id (generate-identifier $id))
      (scope
        (syntax-scope+undefined $renamed-id-lookup $id $renamed-id)
        (syntax-scope+undefined $id-value-lookup $renamed-id (pair $renamed-id $value)))))

  (define (scope-ref? (scope $renamed-id-lookup $id-value-lookup) $id)
    (lets
      ($renamed-id (or (syntax-scope-ref $renamed-id-lookup $id) $id))
      (syntax-scope-ref $id-value-lookup $renamed-id)))

  (define (scope-ref $scope $id)
    (or
      (scope-ref? $scope $id)
      (syntax-error "undefined" $id)))

  (define (scope-commit $scope)
    (id-value-lookup->scope (partial scope-ref? $scope)))
)
