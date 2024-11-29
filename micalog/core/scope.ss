(library (micalog core scope)
  (export
    scope scope? scope-ids scope-values
    empty-scope
    values-scope
    scope-value
    scope+
    scope+gen)
  (import
    (micascheme)
    (prefix (syntax scope) syntax-))

  (data (scope ids values))

  (define (values-scope $values)
    (scope (syntax-empty-scope) $values))

  (define (empty-scope)
    (values-scope (syntax-empty-scope)))

  (define (scope-value (scope $ids $values) $id)
    (syntax-scope-item $values
      (or (syntax-scope-ref $ids $id) $id)))

  (define (scope+ (scope $ids $values) $id $value)
    (scope $ids (syntax-scope+undefined $values $id $value)))

  (define (scope+gen (scope $ids $values) $id $value)
    (lets
      ($gen-id (generate-identifier $id))
      (pair
        (scope
          (syntax-scope+ $ids $id $gen-id)
          (syntax-scope+ $values $gen-id $value))
        $gen-id)))
)
