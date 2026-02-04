(library (micalang compiler)
  (export
    mica-compile
    mica-evaluate

    check-compiles
    check-compile-raises)
  (import
    (micalang base)
    (micalang term)
    (micalang typed))

  (define (mica-environment $fast?)
    (if $fast?
      (environment '(micalang runtime))
      (environment '(micalang runtime-term))))

  (define (mica-evaluate-typed $fast? $env $term)
    (eval
      (typed-ref (mica-compile $fast? $env $term))
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

  (define (mica-compile $fast? $env $term)
    (syntax-case $term (typed lambda :)
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
                  $fast?
                  (cons `(,$symbol ,(typed $type $symbol)) $env)
                  #'body))
              ($body-type (typed-type $typed-body))
              (typed
                (pi $type (lambda (_) $body-type))
                (if $fast?
                  `(lambda (,$symbol) ,(typed-ref $typed-body))
                  `(abstraction (lambda (,$symbol) ,(typed-ref $typed-body)))))))
          ((else _)
            (syntax-error #'id "not identifier"))))
      ((fn arg)
        (lets
          ($typed-fn (mica-compile $fast? $env #'fn))
          ($typed-arg (mica-compile $fast? $env #'arg))
          (switch (typed-type $typed-fn)
            ((pi? $pi)
              (typed
                ((pi-procedure $pi) (typed-type $typed-arg))
                (if $fast?
                  `(app
                    ,(typed-ref $typed-fn)
                    ,(typed-ref $typed-arg))
                  `(app
                    ,(typed-ref $typed-fn)
                    ,(typed-ref $typed-arg)))))
            ((else _)
              (syntax-error #'fn "not function")))))))

  (define-rule-syntax (check-compiles (id expr) ... in out)
    (lets
      ($typed (mica-compile #t `((id ,expr) ...) 'in))
      (check
        (equal?
          `(typed
            ,(term->datum (typed-type $typed))
            ,(typed-ref $typed))
          'out))))

  (define-rule-syntax (check-compile-raises (id expr) ... in)
    (check (raises (mica-compile #t `((id ,expr) ...) 'in))))
)
