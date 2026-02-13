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

  (define-rule-syntax (compiler-comptime compiler datum)
    (eval 'datum (compiler-comptime-environment compiler)))

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
    (throw reify $term))

  (define (default-compiler-term-equal? $default $lhs $rhs)
    (throw term-equal? $lhs $rhs))

  (define (compiler-reify $compiler $term)
    (default-ids-reify
      (compiler-default-reify $compiler)
      (environment-symbols (compiler-environment $compiler))
      $term))

  (define (compiler-term-equal? $compiler $lhs $rhs)
    (default-depth-term-equal?
      (compiler-default-term-equal? $compiler)
      (length (environment-symbols (compiler-environment $compiler)))
      $lhs
      $rhs))

  (define (compiler-push $compiler $id $compiled)
    (compiler
      (compiler-recurse $compiler)
      (compiler-default-reify $compiler)
      (compiler-default-term-equal? $compiler)
      (compiler-runtime-environment $compiler)
      (compiler-comptime-environment $compiler)
      (environment-push (compiler-environment $compiler) $id $compiled)))

  (define (compiler-push? $compiler $id? $compiled)
    (if $id? (compiler-push $compiler $id? $compiled) $compiler))

  (define (compiler-type-ref? $compiler $id)
    (environment-type? (compiler-environment $compiler) $id))

  (define (compiler-evaluate-comptime $compiler $code)
    (lets
      ($environment (compiler-environment $compiler))
      ($code
        (fold-left
          (lambda ($nested $symbol)
            `(lambda ,$symbol ,$nested))
          $code
          (environment-symbols $environment)))
      ($proc (eval $code (compiler-comptime-environment $compiler)))
      (fold-left
        (lambda ($proc $value) (term-apply $proc $value))
        $proc
        (reverse (environment-values $environment)))))

  (define (compiler-compile-type $compiler $term)
    (compiler-compile-typed $compiler a-type $term))

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
          (format "found ~s, expected ~s, in"
            (compiler-reify $compiler $type)
            (compiler-reify $compiler $expected-type))))))

  (define (compiler-compile $compiler $term)
    ((compiler-recurse $compiler) $compiler $term))

  (define (compiler-compile-default $compiler $term)
    (switch $term
      ((compiled? $compiled) $compiled)
      ((else _)
        (syntax-case $term (a-type a-symbol a-boolean a-number a-char a-string quote native native-lambda define expect lambda a-lambda let if macro)
          (a-type (compiled a-type 'a-type))
          (a-symbol (compiled a-type 'a-symbol))
          (a-boolean (compiled a-type 'a-boolean))
          (a-number (compiled a-type 'a-number))
          (a-char (compiled a-type 'a-char))
          (a-string (compiled a-type 'a-string))

          (b
            (boolean? (datum b))
            (compiled a-boolean `(native ,(datum b))))

          (n
            (number? (datum n))
            (compiled a-number `(native ,(datum n))))

          (ch
            (char? (datum ch))
            (compiled a-char `(native ,(datum ch))))

          (s
            (string? (datum s))
            (compiled a-string `(native ,(datum s))))

          ('s
            (symbol? (datum s))
            (compiled a-symbol `(native ',(datum s))))

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
                (compiled $type? (datum id))
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
                (datum v))))

          ((native . _)
            (syntax-error $term))

          ((native-lambda id param ... body)
            (lets
              ($native-symbol (symbol-append '%% (datum id)))
              ($binders
                (map-with
                  ($index (indices (length #'(param ...))))
                  ($param #'(param ...))
                  `(
                    ,(string->symbol (string-append "$" (number->string $index)))
                    ,(syntax->datum $param))))
              (compiler-compile $compiler
                `(native
                  (a-lambda ,@#'(param ...) ,#'body)
                  (curry ,$native-symbol ,@$binders)))))

          ((native-lambda . _)
            (syntax-error $term))

          ((a-lambda out)
            (compiler-compile-default $compiler #'out))

          ((a-lambda param body)
            (lets
              ((values $symbol? $compiled-param) (compiler-compile-param $compiler #'param))
              ($param-type (compiled-type $compiled-param))
              ($param-term (compiled-ref $compiled-param))
              ($param (compiler-evaluate-comptime $compiler $param-term))
              ($compiler (compiler-push? $compiler $symbol? (compiled $param-type $param)))
              ($compiled-body (compiler-compile $compiler #'body))
              ($body-term (compiled-ref $compiled-body))
              (compiled a-type
                `(a-lambda
                  ,(if $symbol? `(,$symbol? ,$param-term) $param-term)
                  ,$body-term))))

          ((a-lambda x xs ... body)
            (compiler-compile-default $compiler
              `(a-lambda ,#'x (a-lambda ,@#'(xs ...) ,#'body))))

          ((a-lambda . _)
            (syntax-error $term))

          ((let body)
            (compiler-compile-default $compiler #'body))

          ((let (define id x) body)
            (compiler-compile $compiler #'(let (id x) body)))

          ((let (expect id t) body)
            (compiler-compile $compiler #'(lambda id body)))

          ((let (id x) body)
            (lets
              ($symbol (datum id))
              ($compiled-x (compiler-compile $compiler #'x))
              ($x-type (compiled-type $compiled-x))
              ($x (compiled-ref $compiled-x))
              ($x-val (compiler-evaluate-comptime $compiler $x))
              ($compiler (compiler-push $compiler $symbol (compiled $x-type $x-val)))
              ($compiled-body (compiler-compile $compiler #'body))
              ($body-type (compiled-type $compiled-body))
              ($body (compiled-ref $compiled-body))
              (compiled
                $body-type
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
              ((values $symbol? $compiled-param) (compiler-compile-param $compiler #'param))
              ($param-term (compiled-ref $compiled-param))
              ($param (compiler-evaluate-comptime $compiler $param-term))
              ($compiled-body
                (compiler-compile
                  (compiler-push? $compiler $symbol?
                    (compiled $param
                      ; TODO: Should it be constant something else?
                      (constant $symbol?)))
                  #'body))
              ($body-type (compiled-type $compiled-body))
              ($body (compiled-ref $compiled-body))
              (compiled
                (type-abstraction $symbol? $param
                  (lambda ($x)
                    (compiled-type
                      (compiler-compile
                        (compiler-push? $compiler $symbol? (compiled $param $x))
                        #'body))))
                `(lambda ,$symbol? ,$body))))

          ((lambda x xs ... body)
            (compiler-compile-default $compiler
              `(lambda ,#'x (lambda ,@#'(xs ...) ,#'body))))

          ((lambda . _)
            (syntax-error $term))

          ((if cond true false)
            (lets
              ($cond (compiler-compile-typed $compiler (compiler-comptime $compiler a-boolean) #'cond))
              ($compiled-true (compiler-compile $compiler #'true))
              ($type (compiled-type $compiled-true))
              ($true (compiled-ref $compiled-true))
              ($false (compiler-compile-typed $compiler $type #'false))
              (compiled $type
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
                ((type-abstraction? $type-abstraction)
                  (lets
                    ($fn-term (compiled-ref $compiled-fn))
                    ($param-type (type-abstraction-param $type-abstraction))
                    ($arg-term (compiler-compile-typed $compiler $param-type #'arg))
                    ($arg-value (compiler-evaluate-comptime $compiler $arg-term))
                    (compiled
                      (type-abstraction-apply $type-abstraction $arg-value)
                      `(app ,$fn-term ,$arg-term))))
                ((else $other)
                  (syntax-error #'fn
                    (format "found ~s, expected pi, in"
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
