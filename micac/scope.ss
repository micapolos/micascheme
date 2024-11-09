(library (micac scope)
  (export scope scope+ scope-ref scope-alloc scope-transform)
  (import (micascheme))

  (define scope list)

  (define (scope+ $scope $id $transformer)
    (push $scope (cons $id $transformer)))

  (define (scope-alloc $scope $id)
    (lets
      ($tmp (car (generate-temporaries (list $id))))
      (scope+ $scope $id (lambda ($syntax) $tmp))))

  (define (scope-ref $scope $id)
    (lets
      ($ass (assid $id $scope))
      (and $ass (cdr $ass))))

  (define (scope-transform $scope $id $syntax)
    (lets
      ($transformer (scope-ref $scope $id))
      (if $transformer
        (transform $transformer $id)
        (syntax-error $id "not bound"))))
)
