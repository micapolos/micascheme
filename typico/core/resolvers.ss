(library (typico core resolvers)
  (export
    predicate-type->resolver

    boolean-resolver
    integer-resolver
    char-resolver
    string-resolver
    symbol-resolver
    pair-resolver

    check-resolves
    check-does-not-resolve)
  (import
    (typico base)
    (typico type)
    (typico fragment)
    (typico core types)
    (typico typed)
    (typico resolver))

  (define (resolve-value? $resolver $type $value)
    (lets
      ($typed (resolve $resolver $value))
      (and
        (type=? (typed-type $typed) $type)
        (typed-value $typed))))

  (define (predicate-type->resolver $predicate $type)
    (resolver ($resolver $value)
      (syntax-case? $value ()
        (value
          (and
            ($predicate (datum value))
            (typed $type (pure-fragment (datum value))))))))

  (define boolean-resolver (predicate-type->resolver boolean? boolean-type))
  (define integer-resolver (predicate-type->resolver (and? integer? exact?) integer-type))
  (define char-resolver    (predicate-type->resolver char? char-type))
  (define string-resolver  (predicate-type->resolver string? string-type))
  (define symbol-resolver  (predicate-type->resolver symbol? symbol-type))

  (define null-resolver
    (resolver ($resolver $value)
      (syntax-case? $value ()
        (()
          (typed '() (pure-fragment '()))))))

  (define pair-resolver
    (resolver ($resolver $value)
      (syntax-case? $value ()
        ((a . b)
          (lets
            ($typed-a (resolve $resolver #'a))
            ($typed-b (resolve $resolver #'b))
            (typed
              (cons
                (typed-type $typed-a)
                (typed-type $typed-b))
              (fragment-bind-with
                ($a (typed-value $typed-a))
                ($b (typed-value $typed-b))
                (pure-fragment `(,$a . ,$b)))))))))

  (define (typed->test-datum $typed)
    (lets
      ($fragment (typed-value $typed))
      `(
        ,@(fragment-imports $fragment)
        ,(type->datum (typed-type $typed))
        ,(fragment-obj $fragment))))

  (define-rule-syntax (check-resolves resolver in out)
    (check
      (equal?
        (switch (resolve? resolver (datum/annotation in))
          ((typed? $typed) (typed->test-datum $typed))
          ((else _) (syntax-error #'in "did not resolve")))
        'out)))

  (define-rule-syntax (check-does-not-resolve resolver in)
    (switch (resolve? resolver (datum/annotation in))
      ((typed? $typed) (syntax-error #'in "should not resolve"))
      ((else _) (void))))

)
