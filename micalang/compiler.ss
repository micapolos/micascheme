(library (micalang compiler)
  (export
    compiler compiler?
    compiler-recurse
    compiler-default-reify
    compiler-default-term-equal?
    compiler-runtime-environment
    compiler-comptime-environment
    compiler-environment

    empty-compiler
    compiler-push

    compiler-compile
    compiler-compile-default
    compiler-reify
    compiler-evaluate

    default-compiler-recurse
    default-compiler-reify
    default-compiler-term-equal?

    check-compiles
    check-compile-raises)
  (import
    (micalang base)
    (micalang term)
    (micalang reify)
    (micalang compiled)
    (micalang environment))

  (data (compiler recurse default-reify default-term-equal? runtime-environment comptime-environment environment))

  (define (empty-compiler $runtime-environment $comptime-environment)
    (compiler
      default-compiler-recurse
      default-compiler-reify
      default-compiler-term-equal?
      $runtime-environment
      $comptime-environment
      (environment)))

  (define (default-compiler-recurse $compiler $term)
    (compiler-compile-default $compiler $term))

  (define (default-compiler-reify $default $term)
    (throw 'reify))

  (define (default-compiler-term-equal? $default $lhs $rhs)
    #f)

  (define (compiler-reify $compiler $term)
    (default-reify (compiler-default-reify $compiler) $term))

  (define (compiler-term-equal? $compiler $lhs $rhs)
    (default-term-equal? (compiler-default-term-equal? $compiler) $lhs $rhs))

  (define (compiler-push $compiler $id $compiled)
    (compiler
      (compiler-recurse $compiler)
      (compiler-default-reify $compiler)
      (compiler-default-term-equal? $compiler)
      (compiler-runtime-environment $compiler)
      (compiler-comptime-environment $compiler)
      (environment-push (compiler-environment $compiler) $id $compiled)))

  (define (compiler-type-ref? $compiler $id)
    (environment-type? (compiler-environment $compiler) $id))

  (define (compiler-evaluate-comptime $compiler $code)
    (lets
      ($environment (compiler-environment $compiler))
      ($code
        (fold-left
          (lambda ($nested $symbol $type-term)
            `(lambda (,$symbol ,$type-term) ,$nested))
          $code
          (environment-symbols $environment)
          (environment-type-terms $environment)))
      ($proc (eval $code (compiler-comptime-environment $compiler)))
      (fold-left
        (lambda ($proc $value) (term-apply $proc $value))
        $proc
        (reverse (environment-values $environment)))))

  (define (compiler-compile-type $compiler $term)
    (compiler-compile-typed $compiler type $term))

  (define (compiler-evaluate $compiler $term)
    (eval
      (compiled-ref (compiler-compile $compiler $term))
      (compiler-runtime-environment $compiler)))

  (define (compiler-compile-typed $compiler $expected-type $term)
    (lets
      ($compiled (compiler-compile $compiler $term))
      ($type (compiled-type $compiled))
      (if (compiler-term-equal? $compiler $type $expected-type)
        (compiled-ref $compiled)
        (syntax-error $term
          (format "invalid type ~s, expected ~s, in"
            (compiler-reify $compiler $type)
            (compiler-reify $compiler $expected-type))))))

  (define (compiler-compile $compiler $term)
    ((compiler-recurse $compiler) $compiler $term))

  (define (compiler-compile-default $compiler $term)
    (switch $term
      ((compiled? $compiled) $compiled)
      ((else _)
        (syntax-case $term (type quote native lambda pi let if macro)
          (type (compiled type 'type 'type))

          (b
            (boolean? (datum b))
            (compiled
              (eval 'boolean (compiler-comptime-environment $compiler))
              'boolean
              `(native ,(datum b))))

          (n
            (number? (datum n))
            (compiled
              (eval 'number (compiler-comptime-environment $compiler))
              'number
              `(native ,(datum n))))

          (ch
            (char? (datum ch))
            (compiled
              (eval 'char (compiler-comptime-environment $compiler))
              'char
              `(native ,(datum ch))))

          (s
            (string? (datum s))
            (compiled
              (eval 'string (compiler-comptime-environment $compiler))
              'string
              `(native ,(datum s))))

          ((quote s)
            (symbol? (datum s))
            (compiled
              (eval 'symbol (compiler-comptime-environment $compiler))
              'symbol
              `(native ',(datum s))))

          (id
            (and
              (symbol? (datum id))
              (macro? (compiler-type-ref? $compiler (datum id))))
            (compiler-compile $compiler
              (macro-apply
                (compiler-type-ref? $compiler (datum id))
                $compiler
                $term)))

          (id
            (symbol? (datum id))
            (lets
              ($type? (compiler-type-ref? $compiler (datum id)))
              (if $type?
                (compiled $type? (compiler-reify $compiler $type?) (datum id))
                (syntax-error #'id "undefined"))))

          ((id . x)
            (and
              (symbol? (datum id))
              (macro? (compiler-type-ref? $compiler (datum id))))
            (compiler-compile $compiler
              (macro-apply
                (compiler-type-ref? $compiler (datum id))
                $compiler
                $term)))

          ((native t v)
            (lets
              ($compiled-t (compiler-compile $compiler #'t))
              ($t-value (compiler-evaluate-comptime $compiler (compiled-ref $compiled-t)))
              (compiled
                $t-value
                (compiled-ref $compiled-t)
                (datum v))))

          ((native . _)
            (syntax-error $term))

          ((pi out)
            (compiler-compile-default $compiler #'out))

          ((pi param body)
            (lets
              ((values $symbol? $compiled-in) (compiler-compile-param $compiler #'param))
              ($in-type (compiled-type $compiled-in))
              ($in-term (compiled-ref $compiled-in))
              ($in-value (compiler-evaluate-comptime $compiler $in-term))
              ($compiler
                (if $symbol?
                  (compiler-push $compiler $symbol? (compiled $in-type $in-term $in-value))
                  $compiler))
              ($compiled-out (compiler-compile $compiler #'body))
              ($out-term (compiled-ref $compiled-out))
              (compiled
                type
                'type
                `(pi
                  ,(if $symbol? `(,$symbol? ,$in-term) $in-term)
                  ,$out-term))))

          ((pi x xs ... body)
            (compiler-compile-default $compiler
              `(pi ,#'x (pi ,@#'(xs ...) ,#'body))))

          ((pi . _)
            (syntax-error $term))

          ((let body)
            (compiler-compile-default $compiler #'body))

          ((let (id x) body)
            (lets
              ($symbol (datum id))
              ($compiled-x (compiler-compile $compiler #'x))
              ($x-type (compiled-type $compiled-x))
              ($x-type-term (compiled-type-term $compiled-x))
              ($x (compiled-ref $compiled-x))
              ($x-val (compiler-evaluate-comptime $compiler $x))
              ($compiler (compiler-push $compiler $symbol (compiled $x-type $x-type-term $x-val)))
              ($compiled-body (compiler-compile $compiler #'body))
              ($body-type (compiled-type $compiled-body))
              ($body (compiled-ref $compiled-body))
              (compiled
                $body-type
                (compiled-type-term $compiled-body)
                `(let (,$symbol ,$x) ,$body))))

          ((let (id param params ... lambda-body) body)
            (compiler-compile-default $compiler
              `(let
                (,#'id (lambda ,#'param ,@#'(params ...) ,#'lambda-body))
                ,#'body)))

          ((let x xs ... body)
            (compiler-compile-default $compiler
              `(let ,#'x (let ,@#'(xs ...) ,#'body))))

          ((let . _)
            (syntax-error $term))

          ((lambda body)
            (compiler-compile-default $compiler #'body))

          ((lambda param body)
            (lets
              ((values $symbol? $compiled-t) (compiler-compile-param $compiler #'param))
              ($t-value (compiler-evaluate-comptime $compiler (compiled-ref $compiled-t)))
              ($body-compiler
                (if $symbol?
                  (compiler-push $compiler $symbol? (compiled $t-value (compiled-ref $compiled-t) (variable $symbol?)))
                  $compiler))
              ($compiled-body (compiler-compile $body-compiler #'body))
              ($body-type (compiled-type $compiled-body))
              ($body (compiled-ref $compiled-body))
              (compiled
                (pi $symbol? $t-value
                  (lambda ($x)
                    (compiler-evaluate-comptime
                      (if $symbol?
                        (compiler-push $compiler $symbol? (compiled $t-value (compiled-ref $compiled-t) $x))
                        $compiler)
                      (compiled-type-term $compiled-body))))
                `(pi
                  ,(if $symbol? `(,$symbol? ,(compiled-ref $compiled-t)) (compiled-ref $compiled-t))
                  ,(compiled-type-term $compiled-body))
                `(lambda (,$symbol? ,(compiled-ref $compiled-t)) ,$body))))

          ((lambda x xs ... body)
            (compiler-compile-default $compiler
              `(lambda ,#'x (lambda ,@#'(xs ...) ,#'body))))

          ((lambda . _)
            (syntax-error $term))

          ((if cond true false)
            (lets
              ($cond (compiler-compile-typed $compiler (eval 'boolean (compiler-comptime-environment $compiler)) #'cond))
              ($compiled-true (compiler-compile $compiler #'true))
              ($type (compiled-type $compiled-true))
              ($true (compiled-ref $compiled-true))
              ($false (compiler-compile-typed $compiler $type #'false))
              (compiled
                $type
                (compiled-type-term $compiled-true)
                `(if ,$cond ,$true ,$false))))

          ((if . _)
            (syntax-error $term))

          ((macro (compiler-id term-id) body)
            (and
              (symbol? (datum compiler-id))
              (symbol? (datum term-id)))
            (compiled
              (macro
                (compiler-evaluate-comptime $compiler
                  `(%%lambda (,#'compiler-id ,#'term-id) ,#'body)))
              (syntax->datum $term)
              `(native #f)))

          ((macro . _)
            (syntax-error $term))

          ((fn)
            (compiler-compile-default $compiler #'fn))

          ((fn arg)
            (lets
              ($compiled-fn (compiler-compile $compiler #'fn))
              ($fn-type (compiled-type $compiled-fn))
              (switch $fn-type
                ((pi? $pi)
                  (lets
                    ($fn-term (compiled-ref $compiled-fn))
                    ($param-type (pi-param $pi))
                    ($arg-term (compiler-compile-typed $compiler $param-type #'arg))
                    ($arg-value (compiler-evaluate-comptime $compiler $arg-term))
                    (compiled
                      (pi-apply $pi $arg-value)
                      (compiler-reify $compiler (pi-apply $pi $arg-value))
                      `(app ,$fn-term ,$arg-term))))
                ((else $other)
                  (syntax-error #'fn
                    (format "invalid type ~s, expected pi, in"
                      (compiler-reify $compiler $other)))))))

          ((fn arg args ...)
            (compiler-compile-default $compiler
              `((,#'fn ,#'arg) ,@#'(args ...))))))))

  (define (compile-id $term)
    (syntax-case $term ()
      (id (symbol? (datum id)) (datum id))
      (_ (syntax-error #'id "not identifier"))))

  (define (compiler-compile-param $compiler $binder)
    (syntax-case $binder (val)
      ((id type)
        (values (compile-id #'id) (compiler-compile $compiler #'type)))
      (type
        (values #f (compiler-compile $compiler #'type)))))

  (define check-runtime-environment
    (%environment '(micalang runtime)))

  (define check-comptime-environment
    (%environment '(micalang comptime)))

  (define-rule-syntax (check-compiles in out)
    (lets
      ($compiled
        (compiler-compile
          (compiler
            default-compiler-recurse
            default-compiler-reify
            default-compiler-term-equal?
            check-runtime-environment
            check-comptime-environment
            mica-environment)
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
        (compiler-compile
          (compiler
            default-compiler-recurse
            default-compiler-reify
            default-compiler-term-equal?
            check-runtime-environment
            check-comptime-environment
            mica-environment)
          'in))))
)
