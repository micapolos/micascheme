(library (typed syntax)
  (export
    syntax->eval
    syntax->typed
    syntax->type)
  (import
    (micascheme)
    (any)
    (syntax lookup)
    (typed type)
    (typed typed)
    (typed keywords))

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
          ($typed-args (map (partial syntax->typed $type-eval $type-lookup) (syntaxes args ...)))
          (switch (typed-type $typed-target)
            ((any-lambda? $any-lambda)
              (typed
                (type-apply $any-lambda (map typed-type $typed-args))
                #`(
                  #,(typed-value $typed-target)
                  #,@(map typed-value $typed-args))))
            ((else $type)
              (syntax-error (typed-value $typed-target) (format "not lambda, but ~s" $type))))))))

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
