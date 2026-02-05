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
          ((lambda (id t) ... body)
            (lets
              ($symbols (map syntax->datum #'(id ...)))
              ($types (map (partial evaluate-type $env) #'(t ...)))
              ($typed-symbols (map typed $types $symbols))
              ($entries (map list $symbols $typed-symbols))
              ($env (append $entries $env))
              ($typed-body (mica-compile $env #'body))
              ($body-type (typed-type $typed-body))
              (typed
                (fold-left
                  (lambda ($acc $type) (pi $type (lambda (_) $acc)))
                  $body-type
                  (reverse $types))
                (fold-left
                  (lambda ($acc $symbol) `(lambda ,$symbol ,$acc))
                  (typed-ref $typed-body)
                  (reverse $symbols)))))
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
          ((let (id x) ... body)
            (lets
              ($symbols (map syntax->datum #'(id ...)))
              ($typed-xs (map (partial mica-compile $env) #'(x ...)))
              ($types (map typed-type $typed-xs))
              ($typed-symbols (map typed $types $symbols))
              ($entries (map list $symbols $typed-symbols))
              ($env (append $entries $env))
              ($typed-body (mica-compile $env #'body))
              ($body-type (typed-type $typed-body))
              (typed $body-type
                `(let
                  ,@(map list $symbols (map typed-ref $typed-xs))
                  ,(typed-ref $typed-body)))))
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
              `(
                ,(mica-compile $env `(,#'fn ,#'arg))
                ,@#'(args ...))))))))

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
