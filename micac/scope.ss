(library (micac scope)
  (export scope scope+ scope-ref)
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
)
