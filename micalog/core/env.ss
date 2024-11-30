(library (micalog core env)
  (export
    env env? env-gen-params? env-scope
    empty-env
    scope->env
    env-value
    env+value
    env+param)
  (import
    (micascheme)
    (micalog core scope))

  (data (env gen-params? scope))

  (define (empty-env)
    (scope->env (empty-scope)))

  (define (scope->env $scope)
    (env (list) $scope))

  (define (env-value $env $id)
    (scope-ref (env-scope $env) $id))

  (define (env+value (env $gen-params? $scope) $id $value)
    (env $gen-params?
      (if (and $gen-params? (not (find (partial free-identifier=? $id) $gen-params?)))
        (scope+gen $scope $id $value)
        (scope+ $scope $id $value))))

  (define (env+param (env $gen-params? $scope) $param)
    (env
      (and $gen-params? (cons $param $gen-params?))
      $scope))
)
