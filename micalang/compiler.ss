(library (micalang compiler)
  (export
    compiler
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

  (define (empty-compiler $runtime-environment $comptime-environment)
    (compiler $runtime-environment $comptime-environment '() '()))

  (define (compiler-push $compiler $id $value $type)
    (compiler
      (compiler-runtime-environment $compiler)
      (compiler-comptime-environment $compiler)
      (push (compiler-env $compiler) (cons $id $value))
      (push (compiler-context $compiler) (cons $id $type))))

  (define (compiler-type-ref $compiler $syntax)
    (cdr
      (or
        (assq (syntax->datum $syntax) (compiler-context $compiler))
        (syntax-error $syntax "undefined"))))

  (define (evaluate-runtime $compiler $code)
    (lets
      ($env (compiler-env $compiler))
      ($symbols (map car $env))
      ($values (map cdr $env))
      ($nested (fold-right (lambda (s acc) `(lambda ,s ,acc)) $code $symbols))
      ($proc (eval $nested (compiler-runtime-environment $compiler)))
      (fold-left (lambda (f v) (f v)) $proc $values)))

  (define (evaluate-comptime $compiler $code)
    (lets
      ($env (compiler-env $compiler))
      ($symbols (map car $env))
      ($values (map cdr $env))
      ($nested (fold-right (lambda (s acc) `(lambda ,s ,acc)) $code $symbols))
      ($proc (eval $nested (compiler-comptime-environment $compiler)))
      (fold-left (lambda (f v) (term-apply f v)) $proc $values)))

  (define (compile-type $compiler $term)
    (mica-compile-compiled $compiler type $term))

  (define (mica-evaluate $compiler $term)
    (eval
      (compiled-ref (mica-compile $compiler $term))
      (compiler-runtime-environment $compiler)))

  (define (mica-compile-compiled $compiler $expected-type $term)
    (lets
      ($compiled (mica-compile $compiler $term))
      ($type (compiled-type $compiled))
      (if (term-equal? $type $expected-type)
        (compiled-ref $compiled)
        (syntax-error $term
          (format "invalid type ~s, expected ~s, in"
            (reify $type)
            (reify $expected-type))))))

  (define (mica-compile $compiler $term)
    (switch $term
      ((compiled? $compiled) $compiled)
      ((else _)
        (syntax-case $term (type quote native lambda pi let if)
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
            (symbol? (datum id))
            (lets
              ($type (compiler-type-ref $compiler #'id))
              (compiled
                $type
                (reify $type)
                (datum id))))

          ((native t v)
            (lets
              ($compiled-t (mica-compile $compiler #'t))
              ($t-value (evaluate-comptime $compiler (compiled-ref $compiled-t)))
              (compiled
                $t-value
                (compiled-ref $compiled-t)
                `(native ,#'v))))

          ((pi out)
            (mica-compile $compiler #'out))

          ((pi (id in) out)
            (lets
              ($id (datum id))
              ($compiled-in (mica-compile $compiler #'in))
              ($in-type (compiled-type $compiled-in))
              ($in-term (compiled-ref $compiled-in))
              ($in-value (evaluate-comptime $compiler $in-term))
              ($compiler (compiler-push $compiler $id (variable $id) $in-value))
              ($compiled-out (mica-compile $compiler #'out))
              ($out-term (compiled-ref $compiled-out))
              (compiled
                type
                'type
                `(pi (,$id ,$in-term) ,$out-term))))

          ((pi in out)
            (lets
              ($in (compile-type $compiler #'in))
              ($out (compile-type $compiler #'out))
              (compiled
                type
                'type
                `(pi ,$in ,$out))))

          ((pi x xs ... body)
            (mica-compile $compiler
              `(pi ,#'x (pi ,@#'(xs ...) ,#'body))))

          ((let body)
            (mica-compile $compiler #'body))

          ((let (id x) body)
            (lets
              ($symbol (datum id))
              ($compiled-x (mica-compile $compiler #'x))
              ($x-type (compiled-type $compiled-x))
              ($x (compiled-ref $compiled-x))
              ($x-val (evaluate-comptime $compiler $x))
              ($compiler (compiler-push $compiler $symbol $x-val $x-type))
              ($compiled-body (mica-compile $compiler #'body))
              ($body-type (compiled-type $compiled-body))
              ($body (compiled-ref $compiled-body))
              (compiled
                $body-type
                (compiled-type-term $compiled-body)
                `(let (,$symbol ,$x) ,$body))))

          ((let (id param params ... lambda-body) body)
            (mica-compile $compiler
              `(let
                (,#'id (lambda ,#'param ,@#'(params ...) ,#'lambda-body))
                ,#'body)))

          ((let x xs ... body)
            (mica-compile $compiler
              `(let ,#'x (let ,@#'(xs ...) ,#'body))))

          ((lambda body)
            (mica-compile $compiler #'body))

          ((lambda (id t) body)
            (lets
              ($symbol (datum id))
              ($compiled-t (mica-compile $compiler #'t))
              ($t-value (evaluate-comptime $compiler (compiled-ref $compiled-t)))
              ($body-compiler (compiler-push $compiler $symbol (variable $symbol) $t-value))
              ($compiled-body (mica-compile $body-compiler #'body))
              ($body-type (compiled-type $compiled-body))
              ($body (compiled-ref $compiled-body))
              (compiled
                (pi $symbol $t-value
                  (lambda ($x)
                    (evaluate-comptime
                      (compiler
                        (compiler-runtime-environment $compiler)
                        (compiler-comptime-environment $compiler)
                        (push (compiler-env $compiler) (cons $symbol $x))
                        (compiler-context $body-compiler))
                      (compiled-type-term $compiled-body))))
                `(pi (,$symbol ,(compiled-ref $compiled-t))
                  ,(compiled-type-term $compiled-body))
                `(lambda ,$symbol ,$body))))

          ((lambda x xs ... body)
            (mica-compile $compiler
              `(lambda ,#'x (lambda ,@#'(xs ...) ,#'body))))

          ((if cond true false)
            (lets
              ($cond (mica-compile-compiled $compiler (eval 'boolean (compiler-comptime-environment $compiler)) #'cond))
              ($compiled-true (mica-compile $compiler #'true))
              ($type (compiled-type $compiled-true))
              ($true (compiled-ref $compiled-true))
              ($false (mica-compile-compiled $compiler $type #'false))
              (compiled
                $type
                (compiled-type-term $compiled-true)
                `(if ,$cond ,$true ,$false))))

          ((fn)
            (mica-compile $compiler #'fn))

          ((fn arg)
            (lets
              ($compiled-fn (mica-compile $compiler #'fn))
              ($fn-type (compiled-type $compiled-fn))
              (switch $fn-type
                ((pi? $pi)
                  (lets
                    ($fn-term (compiled-ref $compiled-fn))
                    ($param-type (pi-param $pi))
                    ($arg-term (mica-compile-compiled $compiler $param-type #'arg))
                    ($arg-value (evaluate-comptime $compiler $arg-term))
                    (compiled
                      (pi-apply $pi $arg-value)
                      (reify (pi-apply $pi $arg-value))
                      `(app ,$fn-term ,$arg-term))))
                ((else $other)
                  (syntax-error #'fn
                    (format "invalid type ~s, expected pi, in"
                      (reify $other)))))))

          ((fn arg args ...)
            (mica-compile $compiler
              `((,#'fn ,#'arg) ,@#'(args ...))))))))

  (define check-runtime-environment
    (environment '(micalang runtime)))

  (define check-comptime-environment
    (environment '(micalang comptime)))

  (define-rule-syntax (check-compiles in out)
    (lets
      ($compiled
        (mica-compile
          (compiler
            check-runtime-environment
            check-comptime-environment
            mica-env
            mica-context)
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
          (compiler
            check-runtime-environment
            check-comptime-environment
            mica-env
            mica-context)
          'in))))
)
