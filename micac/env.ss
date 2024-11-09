(library (micac env)
  (export
    env env? env-lookup env-scope)
  (import
    (micascheme)
    (micac scope))

  (data (env lookup scope))

  (define (env+ $env $id $transformer)
    (env (env-lookup $env) $id $transformer))

  (define (env-ref $env $id)
    (or
      (scope-ref (env-scope $env) $id)
      (app (env-lookup $env) $id)))

  (define (env-transform $env $id $syntax)
    (lets
      ($transformer (env-ref $env $id))
      (if $transformer
        (transform $transformer $syntax (env-lookup $env))
        (syntax-error $id "no macro"))))
)
