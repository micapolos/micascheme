(library (micalang compiler)
  (export mica-compile)
  (import
    (except (micascheme) pi)
    (micalang term))

  (define mica-environment (environment '(scheme) '(micalang term)))

  (define (mica-evaluate-typed $fast? $env $term)
    (eval
      (typed-ref (mica-compile $fast? '() $env $term))
      mica-environment))

  (define (evaluate-type $env $term)
    (eval
      (lets
        ($typed (mica-compile #f $env $term))
        (cond
          ((term-equal? (typed-type $typed) (native 'type)) (typed-ref $typed))
          (else (syntax-error $term "not type"))))
      mica-environment))

  (define (mica-compile $fast? $env $term)
    (syntax-case $term (typed lambda :)
      (fx
        (fixnum? (datum fx))
        (typed
          (native 'int)
          (if $fast?
            (datum fx)
            `(native ,(datum fx)))))
      (id
        (symbol? (datum id))
        (cadr
          (or
            (assq (datum id) $env)
            (syntax-error #'id "undefined"))))
      ((typed t v)
        (typed
          (evaluate-type $env #'t)
          (if $fast?
            `',#'v
            `(native ',#'v))))
      ((lambda (id : t) body)
        (switch (datum id)
          ((symbol? $symbol)
            (lets
              ($type (evaluate-type $env #'t))
              ($typed-body
                (mica-compile
                  $fast?
                  (cons `(,$symbol ,$type) $env)
                  #'body))
              ($body-type (typed-type $typed-body))
              (typed
                (pi $type (lambda (_) $body-type))
                (if $fast?
                  `(lambda (id) ,(typed-ref $typed-body))
                  `(abstraction (lambda (id) ,(typed-ref $typed-body)))))))
          ((else _)
            (syntax-error #'id "not identifier"))))
      ((fn arg)
        (lets
          ($typed-fn (mica-compile $fast? $env #'fn))
          ($typed-arg (mica-compile $fast? $env #'fn))
          (switch (typed-type $typed-fn)
            ((pi? $pi)
              (typed
                ((pi-procedure $pi) (typed-type $typed-arg))
                (if $fast?
                  `(
                    ,(typed-ref $typed-fn)
                    ,(typed-ref $typed-arg))
                  `(term-apply
                    ,(typed-ref $typed-fn)
                    ,(typed-ref $typed-arg)))))
            ((else _)
              (syntax-error #'fn "not function")))))))
)
