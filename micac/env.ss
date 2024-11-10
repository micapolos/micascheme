(library (micac env)
  (export
    env env? env-lookup env-scope
    lookup-env empty-env
    env-alloc env+ env-ref env-transform)
  (import
    (micascheme)
    (micac scope)
    (micac variable))

  (data (env lookup scope))

  (define (lookup-env $lookup)
    (env $lookup (scope)))

  (define empty-env (lookup-env (lambda _ #f)))

  (define (env-alloc $env $id)
    (env
      (env-lookup $env)
      (scope-alloc (env-scope $env) $id)))


  (define (env+ $env $id $item)
    (env
      (env-lookup $env)
      (scope+ (env-scope $env) $id $item)))

  (define (env-ref $env $id)
    (or
      (scope-ref (env-scope $env) $id)
      (app (env-lookup $env) $id)))

  (define (env-transform $env $id $syntax)
    (lets
      ($item (env-ref $env $id))
      (switch $item
        ((variable? $variable)
          (syntax-error $id "not a macro"))
        ((else $transformer)
          (transform $transformer $syntax (env-lookup $env))))))
)
