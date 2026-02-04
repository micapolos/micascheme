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
    (micalang env))

  (define (mica-environment $comptime?)
    (if $comptime?
      (environment '(micalang runtime))
      (environment '(micalang comptime))))

  (define (mica-evaluate-typed $comptime? $env $term)
    (eval
      (typed-ref (mica-compile $comptime? $env $term))
      mica-environment))

  (define (evaluate-type $env $term)
    (eval
      (lets
        ($typed (mica-compile #f $env $term))
        (cond
          ((term-equal? (typed-type $typed) (native 'type)) (typed-ref $typed))
          (else (syntax-error $term "not type"))))
      (mica-environment #f)))

  (define (mica-evaluate $env $term)
    (eval
      (typed-ref (mica-compile #t $env $term))
      (mica-environment #t)))

  (define (mica-compile $comptime? $env $term)
    (switch $term
      ((typed? $typed) $typed)
      ((else _)
        (syntax-case $term (typed lambda :)
          ; === core forms
          (fx
            (fixnum? (datum fx))
            (typed
              (eval 'int (mica-environment #f))
              `(literal ,(datum fx))))
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
          ((lambda (id : t) body)
            (switch (datum id)
              ((symbol? $symbol)
                (lets
                  ($type (evaluate-type $env #'t))
                  ($typed-body
                    (mica-compile
                      $comptime?
                      (cons `(,$symbol ,(typed $type $symbol)) $env)
                      #'body))
                  ($body-type (typed-type $typed-body))
                  (typed
                    (pi $type (lambda (_) $body-type))
                    `(lambda ,$symbol ,(typed-ref $typed-body)))))
              ((else _)
                (syntax-error #'id "not identifier"))))
          ((fn arg)
            (lets
              ($typed-fn (mica-compile $comptime? $env #'fn))
              ($typed-arg (mica-compile $comptime? $env #'arg))
              (switch (typed-type $typed-fn)
                ((pi? $pi)
                  (typed
                    ((pi-procedure $pi) (typed-type $typed-arg))
                    `(app
                      ,(typed-ref $typed-fn)
                      ,(typed-ref $typed-arg))))
                ((else _)
                  (syntax-error #'fn "not function")))))

          ; === macros
          ((lambda (id : t) params ... body)
            (switch (datum id)
              ((symbol? $symbol)
                (lets
                  ($type (evaluate-type $env #'t))
                  ($inner-typed
                    (mica-compile $comptime?
                      (cons `(,$symbol ,(typed $type $symbol)) $env)
                      `(lambda ,@#'(params ...) ,#'body)))
                  ($inner-type (typed-type $inner-typed))
                  (typed
                    (pi $type (lambda (_) $inner-type))
                    `(lambda ,$symbol ,(typed-ref $inner-typed)))))
              ((else _)
                (syntax-error #'id "not identifier"))))

          ((fn arg args ...)
            (mica-compile $comptime? $env
              `(
                ,(mica-compile $comptime? $env `(,#'fn ,#'arg))
                ,@#'(args ...))))))))

  (define-rule-syntax (check-compiles (id expr) ... in out)
    (lets
      ($typed (mica-compile #t `((id ,expr) ... ,@mica-env) 'in))
      (check
        (equal?
          `(typed
            ,(term->datum (typed-type $typed))
            ,(typed-ref $typed))
          'out))))

  (define-rule-syntax (check-compile-raises (id expr) ... in)
    (check (raises (mica-compile #t `((id ,expr) ... ,@mica-env) 'in))))
)
