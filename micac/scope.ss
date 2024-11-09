(library (micac scope)
  (export scope scope+ scope-ref)
  (import (micascheme))

  (define scope list)

  (define (scope+ $scope $id $transformer)
    (push $scope (cons $id $transformer)))

  (define (scope-ref $scope $id)
    (lets
      ($ass (assid $id $scope))
      (and $ass (cdr $ass))))
)
