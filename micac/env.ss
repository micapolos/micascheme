(library (micac env)
  (export
    env env? env-lookup env-scope
    lookup-env empty-env
    env+
    env-alloc
    env-ref
    env-transformer
    env-transform
    env->lookup)
  (import
    (micascheme)
    (micac scope)
    (micac expr))

  (data (env lookup scope))

  (define (lookup-env $lookup)
    (env $lookup (empty-scope)))

  (define empty-env (lookup-env (lambda _ #f)))

  (define (env+ $env $id $item)
    (env
      (env-lookup $env)
      (scope+ (env-scope $env) $id $item)))

  (define (env-alloc $env $id)
    (lets
      ((pair $scope $expr)
        (scope-alloc (env-scope $env) $id))
      (cons (env (env-lookup $env) $scope) $expr)))

  (define (env-ref $env $id)
    (or
      (scope-ref (env-scope $env) $id)
      (app (env-lookup $env) $id)
      (scope-unbound $id)))

  (define (env-transformer $env $id)
    (or
      (scope-transformer (env-scope $env) $id)
      (switch (app (env-lookup $env) $id)
        ((identifier? _) #f)
        ((else $other) $other))))

  (define (env-transform $env $transformer $syntax)
    (transform $transformer $syntax (env-lookup $env)))

  (define (env->lookup $env)
    (lets
      ($scope-lookup (scope->lookup (env-scope $env)))
      ($env-lookup (env-lookup $env))
      (lambda ($id)
        (or
          ($scope-lookup $id)
          (switch ($env-lookup $id)
            ((identifier? _) #f)
            ((false? _) #f)
            ((else $other) $other))))))
)
