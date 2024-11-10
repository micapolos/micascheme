(library (micac scope)
  (export scope scope+ scope-ref scope-alloc scope-transform)
  (import
    (micascheme)
    (micac variable))

  (define scope list)

  (define (scope+ $scope $id $item)
    (push $scope (cons $id $item)))

  (define (scope-alloc $scope $id)
    (lets
      ($tmp (car (generate-temporaries (list $id))))
      (scope+ $scope $id (lambda ($syntax) $tmp))))

  (define (scope-ref $scope $id)
    (lets
      ($ass (assid $id $scope))
      (and $ass (cdr $ass))))

  (define (scope-transform $scope $id $syntax)
    (switch (scope-ref $scope $id)
      ((variable? $variable)
        (syntax-error $id "not a macro"))
      ((false? _)
        (syntax-error $id "not bound"))
      ((else $transformer)
        (transform $transformer $id))))
)
