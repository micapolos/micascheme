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
    (micalang compiled)
    (micalang env)
    (micalang context))

  (data (compiler runtime-environment comptime-environment env context))

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
    (mica-compile-compiled
      $comptime-environment
      $comptime-environment
      $env
      $context
      type
      $term))

  (define (mica-evaluate $runtime-environment $comptime-environment $env $context $term)
    (eval
      (compiled-ref (mica-compile $runtime-environment $comptime-environment $env $context $term))
      $runtime-environment))

  (define (mica-compile-compiled $runtime-environment $comptime-environment $env $context $expected-type $term)
    (lets
      ($compiled (mica-compile $runtime-environment $comptime-environment $env $context $term))
      ($type (compiled-type $compiled))
      (if (term-equal? $type $expected-type)
        (compiled-ref $compiled)
        (syntax-error $term
          (format "invalid type ~s, expected ~s, in"
            (reify $type)
            (reify $expected-type))))))

  (define (mica-compile $runtime-environment $comptime-environment $env $context $term)
    (switch $term
      ((compiled? $compiled) $compiled)
      ((else _)
        (syntax-case $term (type quote native lambda pi let if)
          (type (compiled type 'type 'type))

          (b
            (boolean? (datum b))
            (compiled
              (eval 'boolean $comptime-environment)
              'boolean
              `(native ,(datum b))))

          (n
            (number? (datum n))
            (compiled
              (eval 'number $comptime-environment)
              'number
              `(native ,(datum n))))

          (ch
            (char? (datum ch))
            (compiled
              (eval 'char $comptime-environment)
              'char
              `(native ,(datum ch))))

          (s
            (string? (datum s))
            (compiled
              (eval 'string $comptime-environment)
              'string
              `(native ,(datum s))))

          ((quote s)
            (symbol? (datum s))
            (compiled
              (eval 'symbol $comptime-environment)
              'symbol
              `(native ',(datum s))))

          (id
            (symbol? (datum id))
            (lets
              ($type
                (cdr
                  (or
                    (assq (datum id) $context)
                    (syntax-error #'id "undefined"))))
              (compiled
                $type
                (reify $type)
                (datum id))))

          ((native t v)
            (lets
              ($compiled-t (mica-compile $runtime-environment $comptime-environment $env $context #'t))
              ($t-value (evaluate-comptime $comptime-environment $env $context (compiled-ref $compiled-t)))
              (compiled
                $t-value
                (compiled-ref $compiled-t)
                `(native ,#'v))))

          ((pi out)
            (mica-compile $runtime-environment $comptime-environment $env $context #'out))

          ((pi (id in) out)
            (lets
              ($id (datum id))
              ($compiled-in (mica-compile $runtime-environment $comptime-environment $env $context #'in))
              ($in-type (compiled-type $compiled-in))
              ($in-term (compiled-ref $compiled-in))
              ($in-value (evaluate-comptime $comptime-environment $env $context $in-term))
              ($env (push $env (cons $id (variable $id))))
              ($context (push $context (cons $id $in-value)))
              ($compiled-out (mica-compile $runtime-environment $comptime-environment $env $context #'out))
              ($out-term (compiled-ref $compiled-out))
              (compiled
                type
                'type
                `(pi (,$id ,$in-term) ,$out-term))))

          ((pi in out)
            (lets
              ($in (compile-type $comptime-environment $env $context #'in))
              ($out (compile-type $comptime-environment $env $context #'out))
              (compiled
                type
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
              ($compiled-x (mica-compile $runtime-environment $comptime-environment $env $context #'x))
              ($x-type (compiled-type $compiled-x))
              ($x (compiled-ref $compiled-x))
              ($x-val (evaluate-comptime $comptime-environment $env $context $x))
              ($env (push $env (cons $symbol $x-val)))
              ($context (push $context (cons $symbol $x-type)))
              ($compiled-body (mica-compile $runtime-environment $comptime-environment $env $context #'body))
              ($body-type (compiled-type $compiled-body))
              ($body (compiled-ref $compiled-body))
              (compiled
                $body-type
                (compiled-type-term $compiled-body)
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
              ($compiled-t (mica-compile $runtime-environment $comptime-environment $env $context #'t))
              ($t-value (evaluate-comptime $comptime-environment $env $context (compiled-ref $compiled-t)))
              ($body-env (push $env (cons $symbol (variable $symbol))))
              ($body-context (push $context (cons $symbol $t-value)))
              ($compiled-body (mica-compile $runtime-environment $comptime-environment $body-env $body-context #'body))
              ($body-type (compiled-type $compiled-body))
              ($body (compiled-ref $compiled-body))
              (compiled
                (pi $symbol $t-value
                  (lambda ($x)
                    (evaluate-comptime
                      $comptime-environment
                      (push $env (cons $symbol $x))
                      $body-context
                      (compiled-type-term $compiled-body))))
                `(pi (,$symbol ,(compiled-ref $compiled-t))
                  ,(compiled-type-term $compiled-body))
                `(lambda ,$symbol ,$body))))

          ((lambda x xs ... body)
            (mica-compile $runtime-environment $comptime-environment $env $context
              `(lambda ,#'x (lambda ,@#'(xs ...) ,#'body))))

          ((if cond true false)
            (lets
              ($cond (mica-compile-compiled $runtime-environment $comptime-environment $env $context (eval 'boolean $comptime-environment) #'cond))
              ($compiled-true (mica-compile $runtime-environment $comptime-environment $env $context #'true))
              ($type (compiled-type $compiled-true))
              ($true (compiled-ref $compiled-true))
              ($false (mica-compile-compiled $runtime-environment $comptime-environment $env $context $type #'false))
              (compiled
                $type
                (compiled-type-term $compiled-true)
                `(if ,$cond ,$true ,$false))))

          ((fn)
            (mica-compile $runtime-environment $comptime-environment $env $context #'fn))

          ((fn arg)
            (lets
              ($compiled-fn (mica-compile $runtime-environment $comptime-environment $env $context #'fn))
              ($fn-type (compiled-type $compiled-fn))
              (switch $fn-type
                ((pi? $pi)
                  (lets
                    ($fn-term (compiled-ref $compiled-fn))
                    ($param-type (pi-param $pi))
                    ($arg-term (mica-compile-compiled $runtime-environment $comptime-environment $env $context $param-type #'arg))
                    ($arg-value (evaluate-comptime $comptime-environment $env $context $arg-term))
                    (compiled
                      (pi-apply $pi $arg-value)
                      (reify (pi-apply $pi $arg-value))
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
      ($compiled
        (mica-compile
          check-runtime-environment
          check-comptime-environment
          mica-env
          mica-context
          'in))
      (check
        (equal?
          `(compiled
            ,(reify (compiled-type $compiled))
            ,(compiled-type-term $compiled)
            ,(compiled-ref $compiled))
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
