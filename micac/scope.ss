(library (micac scope)
  (export
    scope scope? scope-bindings scope-gen?
    empty-scope
    scope+
    scope-alloc
    scope-ref
    scope-transformer
    scope-unbound
    scope-with)
  (import
    (micascheme)
    (micac expr))

  (data (scope bindings gen?))

  (define (scope-with . $bindings)
    (scope $bindings #f))

  (define (empty-scope)
    (apply scope-with (list)))

  (define (scope+ $scope $id $item)
    (scope
      (push
        (scope-bindings $scope)
        (cons $id $item))
      (scope-gen? $scope)))

  (define (scope-alloc $scope $id)
    (lets
      ($expr
        (identifier->expr
          (if (scope-gen? $scope)
            (generate-temporary $id)
            $id)))
      (cons (scope+ $scope $id $expr) $expr)))

  (define (scope-ref $scope $id)
    (lets
      ($ass (assid $id (scope-bindings $scope)))
      (and $ass (cdr $ass))))

  (define (scope-transformer $scope $id)
    (switch (scope-ref $scope $id)
      ((expr? _) #f)
      ((false? _) #f)
      ((else $transformer) $transformer)))

  (define (scope-unbound $id)
    (syntax-error $id "unbound identifier"))
)
