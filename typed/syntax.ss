(library (typed syntax)
  (export
    syntax->eval
    syntax->typed
    syntax->type)
  (import
    (micascheme)
    (any)
    (syntax lookup)
    (evaluator)
    (typed type)
    (typed typed)
    (typed keywords)
    (typed compiler))

  (data (constant value))
  (data (dynamic value))

  (define (syntax->type $lookup $syntax)
    (lets
      ($typed (syntax->typed $lookup $syntax))
      (switch (typed-type $typed)
        ((any-type? $any-type)
          (switch-exhaustive (typed-value $typed)
            ((constant? $constant)
              (constant-value $constant))
            ((dynamic? $dynamic)
              (syntax-error $syntax "non constant type"))))
        ((else $other)
          (syntax-error $syntax "not a type")))))

  (define (syntax->typed-value $lookup $expected-type $syntax)
    (lets
      ($typed (syntax->typed $lookup $syntax))
      ($type (typed-type $typed))
      (case
        ((equal? $type $expected-type)
          (typed-value $typed))
        (else
          (syntax-error $syntax
            (format
              "invalid type ~s, expected ~s in"
              $type
              $expected-type))))))

  (define (syntax->typed-lambda $lookup $syntax)
    (lets
      ($typed (syntax->typed $lookup $syntax))
      (switch (typed-type $typed)
        ((any-lambda? $any-lambda)
          (typed $any-lambda (typed-value $typed)))
        ((else $type)
          (syntax-error $syntax
            (format
              "invalid type ~s, expected any-lambda in"
              $type))))))

  (define (syntax->typed $lookup $syntax)
    (syntax-case $syntax (assume type)
      ((assume t expr)
        (typed
          (syntax->type $lookup #'t)
          (dynamic #'expr)))
      (x
        (identifier? #'x)
        (lookup-ref $lookup #'x))
      (x
        (boolean? (datum x))
        (typed any-boolean #'x))
      (x
        (char? (datum x))
        (typed any-char #'x))
      (x
        (string? (datum x))
        (typed any-string #'x))
      (x
        (fixnum? (datum x))
        (typed any-fixnum #'x))
      (x
        (flonum? (datum x))
        (typed any-flonum #'x))
      ((lambda (param ...) body)
        (lets
          ($typed-params
            (map
              (partial param-syntax->typed $type-eval)
              (syntaxes param ...)))
          ($param-types (map typed-type $typed-params))
          ($param-identifiers (map typed-value $typed-params))
          ($typed-body
            (syntax->typed
              $type-eval
              (fold-left lookup+typed $type-lookup $typed-params)
              #'body))
          (typed
            (make-any-lambda $param-types (typed-type $typed-body))
            #`(lambda
              (#,@(map typed-value $typed-params))
              #,(typed-value $typed-body)))))
      ((target args ...)
        (lets
          ($typed-target (syntax->typed $type-eval $type-lookup #'target))
          ($typed-lambda (syntax->typed-lambda $typed-target))
          ($any-lambda (typed-type $typed-lambda))
          ($param-types (map typed-type $typed-args))
          (run
            (when
              (not (= (length $param-types) (length $typed-args)))
              (syntax-error $syntax
                (format
                  "invalid number of args, expected ~s in"
                  (length $param-types)))))
          ($typed-args
            (map
              (partial syntax->typed-value $lookup)
              $param-types
              (syntaxes args ...)))
          (typed
            (any-lambda-result $any-lambda)
            #`(
              #,(typed-value $typed-target)
              #,@$typed-args))))))

  (define (lookup+typed $type-lookup $typed)
    (lookup+ $type-lookup
      (typed-value $typed)
      (typed-type $typed)))

  (define (param-syntax->typed $type-eval $syntax)
    (syntax-case $syntax ()
      ((type identifier)
        (identifier? #'identifier)
        (typed ($type-eval #'type) #'identifier))
      (else
        (syntax-error $syntax "invalid parameter"))))
)
