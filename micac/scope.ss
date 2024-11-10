(library (micac scope)
  (export
    scope scope+
    scope-ref
    scope-transformer
    scope-unbound)
  (import
    (micascheme)
    (micac variable))

  (define scope list)

  (define (scope+ $scope $id $item)
    (push $scope (cons $id $item)))

  (define (scope-ref $scope $id)
    (lets
      ($ass (assid $id $scope))
      (and $ass (cdr $ass))))

  (define (scope-transformer $scope $id)
    (switch (scope-ref $scope $id)
      ((variable? _) #f)
      ((false? _) #f)
      ((else $transformer) $transformer)))

  ; TODO: Make it a syntax-error
  (define (scope-unbound $id)
    (variable $id))
)
