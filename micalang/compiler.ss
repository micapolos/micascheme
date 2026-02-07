(library (micalang compiler)
  (export
    mica-compile
    mica-evaluate

    check-compiles
    check-compile-raises)
  (import
    (micalang base)
    (micalang term)
    (micalang reify)
    (micalang typed)
    (micalang context))

  (define (evaluate-runtime $runtime-environment $env $code)
    (lets
      ($symbols (map car $env))
      ($values (map cdr $env))
      ($nested (fold-right (lambda (s acc) `(lambda ,s ,acc)) $code $symbols))
      ($proc (eval $nested $runtime-environment))
      (fold-left (lambda (f v) (f v)) $proc $values)))

  (define (evaluate-comptime $comptime-environment $env $context $term)
    (lets
      ($symbols (map car $env))
      ($values (map cdr $env))
      ($code (compile-type $comptime-environment $env $context $term))
      ($nested-lambda (fold-right (lambda (s acc) `(lambda ,s ,acc)) $code $symbols))
      ($proc (eval $nested-lambda $comptime-environment))
      (fold-left (lambda (f v) (term-apply f v)) $proc $values)))

  (define (compile-type $comptime-environment $env $context $term)
    (mica-compile-typed
      $comptime-environment
      $comptime-environment
      $env
      $context
      (eval 'type $comptime-environment)
      $term))

  (define (mica-evaluate $runtime-environment $comptime-environment $env $context $term)
    (eval
      (typed-ref (mica-compile $runtime-environment $comptime-environment $env $context $term))
      $runtime-environment))

  (define (mica-compile-typed $runtime-environment $comptime-environment $env $context $expected-type $term)
    (lets
      ($typed (mica-compile $runtime-environment $comptime-environment $env $context $term))
      ($type (typed-type $typed))
      (if (term-equal? $type $expected-type)
        (typed-ref $typed)
        (syntax-error $term
          (format "invalid type ~s, expected ~s, in"
            (reify $type)
            (reify $expected-type))))))

  (define (mica-compile $runtime-environment $comptime-environment $env $context $term)
    (switch $term
      ((typed? $typed) $typed)
      ((else _)
        (syntax-case $term (quote native lambda pi let if)
          (b
            (boolean? (datum b))
            (typed (eval 'boolean $comptime-environment) `(literal ,(datum b))))

          (n
            (number? (datum n))
            (typed (eval 'number $comptime-environment) `(literal ,(datum n))))

          (ch
            (char? (datum ch))
            (typed (eval 'char $comptime-environment) `(literal ,(datum ch))))

          (s
            (string? (datum s))
            (typed (eval 'string $comptime-environment) `(literal ,(datum s))))

          ((quote s)
            (symbol? (datum s))
            (typed (eval 'symbol $comptime-environment) `(literal ',(datum s))))

          (id
            (symbol? (datum id))
            (typed
              (cdr
                (or
                  (assq (datum id) $context)
                  (syntax-error #'id "undefined")))
              (datum id)))

          ((native t v)
            (typed
              (evaluate-comptime $comptime-environment $env $context #'t)
              `(literal ,#'v)))

          ((pi out)
            (mica-compile $runtime-environment $comptime-environment $env $context #'out))

          ((pi (id in) out)
            (lets
              ($id (datum id))
              ($in (compile-type $comptime-environment $env $context #'in))
              ($env (push $env (cons $id (variable $id))))
              ($context (push $context (cons $id (eval 'type $comptime-environment))))
              ($out (compile-type $comptime-environment $env $context #'out))
              (typed (eval 'type $comptime-environment) `(pi (,$id ,$in) ,$out))))

          ((pi in out)
            (lets
              ($in (compile-type $comptime-environment $env $context #'in))
              ($out (compile-type $comptime-environment $env $context #'out))
              (typed (eval 'type $comptime-environment) `(pi ,$in ,$out))))

          ((pi x xs ... body)
            (mica-compile $runtime-environment $comptime-environment $env $context
              `(pi ,#'x (pi ,@#'(xs ...) ,#'body))))

          ((let body)
            (mica-compile $runtime-environment $comptime-environment $env $context #'body))

          ((let (id x) body)
            (lets
              ($symbol (datum id))
              ($typed-x (mica-compile $runtime-environment $comptime-environment $env $context #'x))
              ($x-type (typed-type $typed-x))
              ($x (typed-ref $typed-x))
              ($x-val (evaluate-runtime $runtime-environment $env $x))
              ($env (push $env (cons $symbol $x-val)))
              ($context (push $context (cons $symbol $x-type)))
              ($typed-body (mica-compile $runtime-environment $comptime-environment $env $context #'body))
              ($body-type (typed-type $typed-body))
              ($body (typed-ref $typed-body))
              (typed $body-type `(let (,$symbol ,$x) ,$body))))

          ((let (id param params ... lambda-body) body)
            (mica-compile $runtime-environment $comptime-environment $env $context
              `(let
                (,#'id (lambda ,#'param ,@#'(params ...) ,#'lambda-body))
                ,#'body)))

          ((let x xs ... body)
            (mica-compile $runtime-environment $comptime-environment $env $context
              `(let ,#'x (let ,@#'(xs ...) ,#'body))))

          ((lambda body)
            (mica-compile $runtime-environment $comptime-environment $env $context #'body))

          ((lambda (id t) body)
            (lets
              ($symbol (datum id))
              ($type (evaluate-comptime $comptime-environment $env $context #'t))
              ($body-env (push $env (cons $symbol (variable $symbol))))
              ($body-context (push $context (cons $symbol $type)))
              ($typed-body (mica-compile $runtime-environment $comptime-environment $body-env $body-context #'body))
              ($body-type (typed-type $typed-body))
              ($body (typed-ref $typed-body))
              (typed
                (pi $symbol $type
                  (lambda ($x)
                    (evaluate-comptime
                      $comptime-environment
                      (push $env (cons $symbol $x))
                      $body-context
                      (reify $body-type))))
                `(lambda ,$symbol ,$body))))

          ((lambda x xs ... body)
            (mica-compile $runtime-environment $comptime-environment $env $context
              `(lambda ,#'x (lambda ,@#'(xs ...) ,#'body))))

          ((if cond true false)
            (lets
              ($cond (mica-compile-typed $runtime-environment $comptime-environment $env $context (eval 'boolean $comptime-environment) #'cond))
              ($typed-true (mica-compile $runtime-environment $comptime-environment $env $context #'true))
              ($type (typed-type $typed-true))
              ($true (typed-ref $typed-true))
              ($false (mica-compile-typed $runtime-environment $comptime-environment $env $context $type #'false))
              (typed $type `(if ,$cond ,$true ,$false))))

          ((fn)
            (mica-compile $runtime-environment $comptime-environment $env $context #'fn))

          ((fn arg)
            (lets
              ($typed-fn (mica-compile $runtime-environment $comptime-environment $env $context #'fn))
              ($fn-type (typed-type $typed-fn))
              (switch $fn-type
                ((pi? $pi)
                  (lets
                    ($fn (typed-ref $typed-fn))
                    ($param-type (pi-param $pi))
                    ($arg (mica-compile-typed $runtime-environment $comptime-environment $env $context $param-type #'arg))
                    ($arg-val (evaluate-runtime $runtime-environment $env $arg))
                    (typed
                      (pi-apply $pi $arg-val)
                      `(app ,$fn ,$arg))))
                ((else $other)
                  (syntax-error #'fn
                    (format "invalid type ~s, expected pi, in"
                      (reify $other)))))))

          ((fn arg args ...)
            (mica-compile $runtime-environment $comptime-environment $env $context
              `((,#'fn ,#'arg) ,@#'(args ...))))))))

  (define check-runtime-environment
    (environment '(micalang runtime)))

  (define check-comptime-environment
    (environment '(micalang comptime)))

  (define-rule-syntax (check-compiles in out)
    (lets
      ($typed (mica-compile check-runtime-environment check-comptime-environment `() `(,@mica-context) 'in))
      (check
        (equal?
          `(,(reify (typed-type $typed)) ,(typed-ref $typed))
          'out))))

  (define-rule-syntax (check-compile-raises in)
    (check (raises (mica-compile check-runtime-environment check-comptime-environment `() `(,@mica-context) 'in))))
)
