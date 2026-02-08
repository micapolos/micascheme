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
    (micalang env)
    (micalang context))

  (define (evaluate-runtime $runtime-environment $env $code)
    (lets
      ($symbols (map car $env))
      ($values (map cdr $env))
      ($nested (fold-right (lambda (s acc) `(lambda ,s ,acc)) $code $symbols))
      ($proc (eval $nested $runtime-environment))
      (fold-left (lambda (f v) (f v)) $proc $values)))

  (define (evaluate-comptime $comptime-environment $env $context $code)
    (lets
      ($symbols (map car $env))
      ($values (map cdr $env))
      ($nested (fold-right (lambda (s acc) `(lambda ,s ,acc)) $code $symbols))
      ($proc (eval $nested $comptime-environment))
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
            (typed
              (eval 'boolean $comptime-environment)
              'boolean
              `(literal ,(datum b))))

          (n
            (number? (datum n))
            (typed
              (eval 'number $comptime-environment)
              'number
              `(literal ,(datum n))))

          (ch
            (char? (datum ch))
            (typed
              (eval 'char $comptime-environment)
              'char
              `(literal ,(datum ch))))

          (s
            (string? (datum s))
            (typed
              (eval 'string $comptime-environment)
              'string
              `(literal ,(datum s))))

          ((quote s)
            (symbol? (datum s))
            (typed
              (eval 'symbol $comptime-environment)
              'symbol
              `(literal ',(datum s))))

          (id
            (symbol? (datum id))
            (typed
              (cdr
                (or
                  (assq (datum id) $context)
                  (syntax-error #'id "undefined")))
              (datum id)
              (datum id)))

          ((native t v)
            (lets
              ($typed-t (mica-compile $runtime-environment $comptime-environment $env $context #'t))
              ($t-value (evaluate-comptime $comptime-environment $env $context (typed-ref $typed-t)))
              (typed
                $t-value
                (typed-ref $typed-t)
                `(literal ,#'v))))

          ((pi out)
            (mica-compile $runtime-environment $comptime-environment $env $context #'out))

          ((pi (id in) out)
            (lets
              ($id (datum id))
              ($typed-in (mica-compile $runtime-environment $comptime-environment $env $context #'in))
              ($in-type (typed-type $typed-in))
              ($in-term (typed-ref $typed-in))
              ($in-value (evaluate-comptime $comptime-environment $env $context $in-term))
              ($env (push $env (cons $id (variable $id))))
              ($context (push $context (cons $id $in-value)))
              ($typed-out (mica-compile $runtime-environment $comptime-environment $env $context #'out))
              ($out-term (typed-ref $typed-out))
              (typed
                (eval 'type $comptime-environment)
                'type
                `(pi (,$id ,$in-term) ,$out-term))))

          ((pi in out)
            (lets
              ($in (compile-type $comptime-environment $env $context #'in))
              ($out (compile-type $comptime-environment $env $context #'out))
              (typed
                (eval 'type $comptime-environment)
                'type
                `(pi ,$in ,$out))))

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
              ($x-val (evaluate-comptime $comptime-environment $env $context $x))
              ($env (push $env (cons $symbol $x-val)))
              ($context (push $context (cons $symbol $x-type)))
              ($typed-body (mica-compile $runtime-environment $comptime-environment $env $context #'body))
              ($body-type (typed-type $typed-body))
              ($body (typed-ref $typed-body))
              (typed
                $body-type
                (typed-type-term $typed-body)
                `(let (,$symbol ,$x) ,$body))))

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
              ($typed-t (mica-compile $runtime-environment $comptime-environment $env $context #'t))
              ($t-value (evaluate-comptime $comptime-environment $env $context (typed-ref $typed-t)))
              ($body-env (push $env (cons $symbol (variable $symbol))))
              ($body-context (push $context (cons $symbol $t-value)))
              ($typed-body (mica-compile $runtime-environment $comptime-environment $body-env $body-context #'body))
              ($body-type (typed-type $typed-body))
              ($body (typed-ref $typed-body))
              (typed
                (pi $symbol $t-value
                  (lambda ($x)
                    (evaluate-comptime
                      $comptime-environment
                      (push $env (cons $symbol $x))
                      $body-context
                      (reify $body-type))))
                `(pi (,$symbol ,(typed-ref $typed-t))
                  ,(typed-type-term $typed-body))
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
              (typed
                $type
                (typed-type-term $typed-true)
                `(if ,$cond ,$true ,$false))))

          ((fn)
            (mica-compile $runtime-environment $comptime-environment $env $context #'fn))

          ((fn arg)
            (lets
              ($typed-fn (mica-compile $runtime-environment $comptime-environment $env $context #'fn))
              ($fn-type (typed-type $typed-fn))
              (switch $fn-type
                ((pi? $pi)
                  (lets
                    ($fn-term (typed-ref $typed-fn))
                    ($param-type (pi-param $pi))
                    ($arg-term (mica-compile-typed $runtime-environment $comptime-environment $env $context $param-type #'arg))
                    ($arg-value (evaluate-comptime $comptime-environment $env $context $arg-term))
                    (typed
                      (pi-apply $pi $arg-value)
                      `(app ,(typed-type-term $typed-fn) ,$arg-term)
                      `(app ,$fn-term ,$arg-term))))
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
      ($typed
        (mica-compile
          check-runtime-environment
          check-comptime-environment
          mica-env
          mica-context
          'in))
      (check
        (equal?
          `(,(reify (typed-type $typed)) ,(typed-ref $typed))
          'out))))

  (define-rule-syntax (check-compile-raises in)
    (check
      (raises
        (mica-compile
          check-runtime-environment
          check-comptime-environment
          mica-env
          mica-context
          'in))))
)
