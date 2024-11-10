(library (micac env)
  (export
    env env? env-lookup env-scope
    lookup-env empty-env
    env+
    env-ref
    env-transformer
    env-transform)
  (import
    (micascheme)
    (micac scope)
    (micac variable))

  (data (env lookup scope))

  (define (lookup-env $lookup)
    (env $lookup (scope)))

  (define empty-env (lookup-env (lambda _ #f)))

  (define (env+ $env $id $item)
    (env
      (env-lookup $env)
      (scope+ (env-scope $env) $id $item)))

  (define (env-ref $env $id)
    (or
      (scope-ref (env-scope $env) $id)
      (app (env-lookup $env) $id)
      (scope-unbound $id)))

  (define (env-transformer $env $id)
    (or
      (scope-transformer (env-scope $env) $id)
      (app (env-lookup $env) $id)))

  (define (env-transform $env $transformer $syntax)
    (transform $transformer $syntax (env-lookup $env)))
)
