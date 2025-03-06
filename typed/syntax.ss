(library (typed syntax)
  (export
    syntax->typed
    type->syntax)
  (import
    (micascheme)
    (syntax lookup)
    (typed type)
    (typed typed)
    (typed keywords))

  (define (syntax->typed $type-eval $type-lookup $syntax)
    (syntax-case $syntax (typeof type assume : lambda)
      ((typeof expr)
        (typed any-type
          (type->syntax
            (typed-type
              (syntax->typed $type-eval $type-lookup #'expr)))))
      ((type x)
        (typed any-type
          (type->syntax ($type-eval #'x))))
      ((assume typ expr)
        (typed ($type-eval #'typ) #'expr))
      ((: typ expr)
        (lets
          ($typed-expr (syntax->typed $type-eval $type-lookup #'expr))
          ($as-type ($type-eval #'typ))
          (if (equal? (typed-type $typed-expr) $as-type)
            $typed-expr
            (syntax-error #'expr
              (format "invalid type: expected ~s, actual ~s in"
                $as-type
                (typed-type $typed-expr))))))
      (x
        (identifier? #'x)
        (switch ($type-lookup #'x)
          ((false? _)
            (syntax-error #'x "not bound"))
          ((else $type)
            (typed $type #'x))))
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

  (define (type->syntax $type)
    (switch-exclusive $type
      ((type? $type)
        #`(type
          #,(type-id-string $type)
          (list #,@(map type->syntax (type-args $type)))))
      ((any-type? $any-type)
        #'any-type)
      ((any-boolean? $any-boolean)
        #'any-boolean)
      ((any-char? $any-char)
        #'any-char)
      ((any-string? $any-string)
        #'any-string)
      ((any-fixnum? $any-fixnum)
        #'any-fixnum)
      ((any-flonum? $any-flonum)
        #'any-flonum)
      ((any-lambda? $any-lambda)
        #`(any-lambda
          (#,@(map type->syntax (any-lambda-params $any-lambda)))
          #,(type->syntax (any-lambda-result $any-lambda))))
      ((any-list? $any-list)
        #`(any-list
          #,(type->syntax (any-list-component $any-list))))))
)
