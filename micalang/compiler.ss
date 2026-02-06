(library (micalang compiler)
  (export
    mica-compile
    mica-evaluate

    check-compiles
    check-compile-raises)
  (import
    (micalang base)
    (micalang term)
    (micalang typed)
    (micalang env)
    (prefix (micalang comptime) comptime-))

  (define (mica-environment $runtime?)
    (if $runtime?
      (environment '(micalang runtime))
      (environment '(micalang comptime))))

  (define (evaluate-type $env $term)
    (eval
      (compile-type $env $term)
      (mica-environment #f)))

  (define (compile-type $env $term)
    (lets
      ($typed (mica-compile $env $term))
      (cond
        ((term-equal? (typed-type $typed) (native 'type))
          (typed-ref $typed))
        (else
          (syntax-error $term "not type")))))

  (define (mica-evaluate $env $term)
    (eval
      (typed-ref (mica-compile $env $term))
      (mica-environment #t)))

  (define (mica-compile $env $term)
    (switch $term
      ((typed? $typed) $typed)
      ((else _)
        (syntax-case $term (typed lambda pi let)
          ; === core forms
          (fx
            (fixnum? (datum fx))
            (typed comptime-int `(literal ,(datum fx))))
          (b
            (boolean? (datum b))
            (typed comptime-bool `(literal ,(datum b))))
          (id
            (symbol? (datum id))
            (cadr
              (or
                (assq (datum id) $env)
                (syntax-error #'id "undefined"))))
          ((typed t v)
            (typed
              (evaluate-type $env #'t)
              `(literal ',#'v)))

          ((pi (id in) out)
            (lets
              ($id (datum id))
              ($in (compile-type $env #'in))
              ($env (push $env `(,$id ,(typed comptime-type $id))))
              ($out (compile-type $env #'out))
              (typed comptime-type `(pi (,$id ,$in) ,$out))))
          ((pi in out)
            (lets
              ($in (compile-type $env #'in))
              ($out (compile-type $env #'out))
              (typed comptime-type `(pi ,$in ,$out))))

          ((let body)
            (mica-compile $env #'body))

          ((let (id x) body)
            (lets
              ($symbol (datum id))
              ($typed-x (mica-compile $env #'x))
              ($x-type (typed-type $typed-x))
              ($x (typed-ref $typed-x))
              ($env (cons `(,$symbol ,(typed $x-type $symbol)) $env))
              ($typed-body (mica-compile $env #'body))
              ($body-type (typed-type $typed-body))
              ($body (typed-ref $typed-body))
              (typed $body-type `(let (,$symbol ,$x) ,$body))))

          ((let x xs ... body)
            (mica-compile $env
              `(let ,#'x (let ,@#'(xs ...) ,#'body))))

          ((lambda body)
            (mica-compile $env #'body))

          ((lambda (id t) body)
            (lets
              ($symbol (datum id))
              ($type (evaluate-type $env #'t))
              ($env (push $env `(,$symbol ,(typed $type $symbol))))
              ($typed-body (mica-compile $env #'body))
              ($body-type (typed-type $typed-body))
              ($body (typed-ref $typed-body))
              (typed
                (pi $type (lambda (_) $body-type))
                `(lambda ,$symbol ,$body))))

          ((lambda x xs ... body)
            (mica-compile $env
              `(lambda ,#'x (lambda ,@#'(xs ...) ,#'body))))

          ; --- application
          ((fn)
            (mica-compile $env #'fn))

          ((fn arg)
            (lets
              ($typed-fn (mica-compile $env #'fn))
              (switch (typed-type $typed-fn)
                ((pi? $pi)
                  (lets
                    ($typed-arg (mica-compile $env #'arg))
                    ($arg-type (typed-type $typed-arg))
                    (if (term-equal? (pi-param $pi) $arg-type)
                      (typed
                        ((pi-procedure $pi) $arg-type)
                        `(app
                          ,(typed-ref $typed-fn)
                          ,(typed-ref $typed-arg)))
                      (syntax-error #'arg
                        (format "invalid type ~s, expected ~s, in"
                          (term->datum $arg-type)
                          (term->datum (pi-param $pi)))))))
                ((else $other)
                  (syntax-error #'fn
                    (format "invalid type ~s, expected pi, in"
                      (term->datum $other)))))))

          ((fn arg args ...)
            (mica-compile $env
              `((,#'fn ,#'arg) ,@#'(args ...))))))))

  (define-rule-syntax (check-compiles in out)
    (lets
      ($typed (mica-compile `(,@mica-env) 'in))
      (check
        (equal?
          `(typed
            ,(term->datum (typed-type $typed))
            ,(typed-ref $typed))
          'out))))

  (define-rule-syntax (check-compile-raises in)
    (check (raises (mica-compile `(,@mica-env) 'in))))
)
