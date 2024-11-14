(library (micac scope)
  (export
    scope scope? scope-bindings scope-size
    empty-scope
    scope+
    scope-alloc
    scope-ref
    scope-transformer
    scope-unbound
    scope-with
    $pretty-identifier?
    scope->lookup)
  (import
    (micascheme)
    (micac expr))

  (define $pretty-identifier? (make-parameter #f))

  (data (scope bindings size))

  (define (scope-with . $bindings)
    (scope $bindings (length $bindings)))

  (define (empty-scope)
    (apply scope-with (list)))

  (define (scope+ $scope $id $item)
    (scope
      (push
        (scope-bindings $scope)
        (cons $id $item))
      (+ (scope-size $scope) 1)))

  (define (scope-alloc $scope $id)
    (lets
      ($identifier
        (if ($pretty-identifier?)
          $id
          (datum->syntax $id
            (string->symbol
              (string-append
                "v"
                (number->string (scope-size $scope))
                "-"
                (symbol->string (syntax->datum $id)))))))
      (pair
        (scope+ $scope $id $identifier)
        $identifier)))

  (define (scope-ref $scope $id)
    (lets
      ($ass (assid $id (scope-bindings $scope)))
      (and $ass (cdr $ass))))

  (define (scope-transformer $scope $id)
    (switch (scope-ref $scope $id)
      ((identifier? _) #f)
      ((false? _) #f)
      ((else $transformer) $transformer)))

  (define (scope-unbound $id)
    (syntax-error $id "unbound identifier"))

  (define (scope->lookup $scope)
    (lambda ($id)
      (switch (scope-ref $scope $id)
        ((identifier? _) #f)
        ((false? _) #f)
        ((else $transformer) $transformer))))
)
