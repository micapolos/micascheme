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
    (micalang context)
    (prefix (micalang comptime) comptime-))

  (define runtime-environment
    (environment '(micalang runtime)))

  (define comptime-environment
    (environment '(micalang comptime)))

  (define (evaluate-type $context $term)
    (eval
      (compile-type $context $term)
      comptime-environment))

  (define (compile-type $context $term)
    (mica-compile-typed $context comptime-type $term))

  (define (mica-evaluate $context $term)
    (eval
      (typed-ref (mica-compile $context $term))
      runtime-environment))

  (define (mica-compile-typed $context $expected-type $term)
    (lets
      ($typed (mica-compile $context $term))
      ($type (typed-type $typed))
      (if (term-equal? $type $expected-type)
        (typed-ref $typed)
        (syntax-error $term
          (format "invalid type ~s, expected ~s, in"
            (reify $type)
            (reify $expected-type))))))

  (define (mica-compile $context $term)
    (switch $term
      ((typed? $typed) $typed)
      ((else _)
        (syntax-case $term (quote native lambda pi let if)
          (b
            (boolean? (datum b))
            (typed comptime-boolean `(literal ,(datum b))))

          (n
            (number? (datum n))
            (typed comptime-number `(literal ,(datum n))))

          (ch
            (char? (datum ch))
            (typed comptime-char `(literal ,(datum ch))))

          (s
            (string? (datum s))
            (typed comptime-string `(literal ,(datum s))))

          ((quote s)
            (symbol? (datum s))
            (typed comptime-symbol `(literal ',(datum s))))

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
              (evaluate-type $context #'t)
              `(literal ,#'v)))

          ((pi out)
            (mica-compile $context #'out))

          ((pi (id in) out)
            (lets
              ($id (datum id))
              ($in (compile-type $context #'in))
              ($context (push $context (cons $id comptime-type)))
              ($out (compile-type $context #'out))
              (typed comptime-type `(pi (,$id ,$in) ,$out))))

          ((pi in out)
            (lets
              ($in (compile-type $context #'in))
              ($out (compile-type $context #'out))
              (typed comptime-type `(pi ,$in ,$out))))

          ((pi x xs ... body)
            (mica-compile $context
              `(pi ,#'x (pi ,@#'(xs ...) ,#'body))))

          ((let body)
            (mica-compile $context #'body))

          ((let (id x) body)
            (lets
              ($symbol (datum id))
              ($typed-x (mica-compile $context #'x))
              ($x-type (typed-type $typed-x))
              ($x (typed-ref $typed-x))
              ($context (push $context (cons $symbol $x-type)))
              ($typed-body (mica-compile $context #'body))
              ($body-type (typed-type $typed-body))
              ($body (typed-ref $typed-body))
              (typed $body-type `(let (,$symbol ,$x) ,$body))))

          ((let (id param params ... lambda-body) body)
            (mica-compile $context
              `(let
                (,#'id (lambda ,#'param ,@#'(params ...) ,#'lambda-body))
                ,#'body)))

          ((let x xs ... body)
            (mica-compile $context
              `(let ,#'x (let ,@#'(xs ...) ,#'body))))

          ((lambda body)
            (mica-compile $context #'body))

          ((lambda (id t) body)
            (lets
              ($symbol (datum id))
              ($type (evaluate-type $context #'t))
              ($context (push $context (cons $symbol $type)))
              ($typed-body (mica-compile $context #'body))
              ($body-type (typed-type $typed-body))
              ($body (typed-ref $typed-body))
              (typed
                (pi 'id $type (lambda (_) $body-type))
                `(lambda ,$symbol ,$body))))

          ((lambda x xs ... body)
            (mica-compile $context
              `(lambda ,#'x (lambda ,@#'(xs ...) ,#'body))))

          ((if cond true false)
            (lets
              ($cond (mica-compile-typed $context comptime-boolean #'cond))
              ($typed-true (mica-compile $context #'true))
              ($type (typed-type $typed-true))
              ($true (typed-ref $typed-true))
              ($false (mica-compile-typed $context $type #'false))
              (typed $type `(if ,$cond ,$true ,$false))))

          ((fn)
            (mica-compile $context #'fn))

          ((fn arg)
            (lets
              ($typed-fn (mica-compile $context #'fn))
              ($fn-type (typed-type $typed-fn))
              (switch $fn-type
                ((pi? $pi)
                  (lets
                    ($fn (typed-ref $typed-fn))
                    ($param-type (pi-param $pi))
                    ($arg (mica-compile-typed $context $param-type #'arg))
                    (typed
                      (pi-apply $pi $param-type)
                      `(app ,$fn ,$arg))))
                ((else $other)
                  (syntax-error #'fn
                    (format "invalid type ~s, expected pi, in"
                      (reify $other)))))))

          ((fn arg args ...)
            (mica-compile $context
              `((,#'fn ,#'arg) ,@#'(args ...))))))))

  (define-rule-syntax (check-compiles in out)
    (lets
      ($typed (mica-compile `(,@mica-context) 'in))
      (check
        (equal?
          `(,(reify (typed-type $typed)) ,(typed-ref $typed))
          'out))))

  (define-rule-syntax (check-compile-raises in)
    (check (raises (mica-compile `(,@mica-context) 'in))))
)
