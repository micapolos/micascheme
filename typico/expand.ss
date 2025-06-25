(library (typico expand)
  (export
    boolean-type
    integer-type
    expand-typed
    expand-typed/no-lookup
    expand-value-of
    expand-predicate-value-of
    type-error
    types-error)
  (import
    (micascheme)
    (typico type)
    (typico core types)
    (typico typed)
    (asm u))

  (define (syntax-id $syntax)
    (syntax-case? $syntax ()
      (id
        (symbol? (datum id))
        #'id)
      ((id . _)
        (symbol? (datum id))
        #'id)))

  (define (expand-typed $lookup $syntax)
    (switch (syntax-id $syntax)
      ((false? _)
        (expand-typed/no-lookup $lookup $syntax))
      ((else $id)
        (switch ($lookup (syntax-case $id () (id (datum id))))
          ((false? _) (syntax-error $id "undefined"))
          ((else $proc) ($proc $lookup $syntax))))))

  (define (expand-typed/no-lookup $lookup $syntax)
    (or
      (expand-typed-literal? $syntax)
      (expand-typed-application? $lookup $syntax)
      (syntax-error $syntax)))

  (define (expand-typed-literal? $syntax)
    (syntax-case? $syntax ()
      (b
        (boolean? (datum b))
        (typed boolean-type (datum b)))
      (i
        (and (integer? (datum i)) (exact? (datum i)))
        (typed integer-type (datum i)))
      (ch
        (char? (datum ch))
        (typed char-type (datum ch)))
      (s
        (string? (datum s))
        (typed string-type (datum s)))))

  (define (expand-typed-application? $lookup $syntax)
    (syntax-case? $syntax ()
      ((fn arg ...)
        (lets
          ((typed $function-type $function-value)
            (expand-typed-function $lookup #'fn))
            (lets
              ($typed-args (map (partial expand-typed $lookup) #'(arg ...)))
              (typed
                (function-type-result-type $function-type)
                `(
                  ,$function-value
                  ,@(typed-arg-values
                    $syntax
                    (function-type-param-types $function-type)
                    $typed-args
                    #'(arg ...)))))))))

  (define (typed-arg-values $syntax $param-types $typed-args $arg-syntaxes)
    (switch $param-types
      ((null? _)
        (cond
          ((null? $typed-args) '())
          (else (syntax-error $syntax "illegal argument count"))))
      ((pair? (pair $param-type $param-types))
        (switch $typed-args
          ((pair? (pair $typed-arg $typed-args))
            (cons
              (typed-value-of (car $arg-syntaxes) $param-type $typed-arg)
              (typed-arg-values $syntax $param-types $typed-args (cdr $arg-syntaxes))))
          ((else _)
            (syntax-error $syntax "illegal argument count"))))
      ((else $vararg-type)
        (map (partial typed-value-of $syntax $vararg-type) $typed-args))))

  (define (typed-value-of $syntax $type $typed)
    (cond
      ((type=? $type (typed-type $typed))
        (typed-value $typed))
      (else
        (type-error $syntax (typed-type $typed) $type))))

  (define (expand-typed-function $lookup $syntax)
    (lets
      ($typed (expand-typed $lookup $syntax))
      (switch (typed-type $typed)
        ((function-type? $function-type) $typed)
        ((else $other-type) (syntax-error #'fn "not a function")))))

  (define (expand-predicate-value-of $lookup $predicate $type $syntax)
    (syntax-case $syntax ()
      (x ($predicate (datum x)) (datum x))
      (other (typed-value-of #'other $type (expand-typed $lookup #'other)))))

  (define (expand-value-of $lookup $type $syntax)
    (syntax-case $syntax ()
      (i
        (integer? (datum i))
        (cond
          ((type=? $type u2-type)
            (expand-predicate-value-of $lookup u2? $type $syntax))
          ((type=? $type u3-type)
            (expand-predicate-value-of $lookup u3? $type $syntax))
          ((type=? $type u7-type)
            (expand-predicate-value-of $lookup u7? $type $syntax))
          ((type=? $type u8-type)
            (expand-predicate-value-of $lookup u8? $type $syntax))
          ((type=? $type u16-type)
            (expand-predicate-value-of $lookup u16? $type $syntax))
          ((type=? $type s8-type)
            (expand-predicate-value-of $lookup s8? $type $syntax))
          (else
            (typed-value-of $syntax $type (expand-typed $lookup $syntax)))))
      (other
        (typed-value-of #'other $type (expand-typed $lookup #'other)))))

  (define (type-error $syntax $actual-type $expected-type)
    (syntax-error $syntax
      (format "invalid type ~s, expected ~s, in"
        (type->datum $actual-type)
        (type->datum $expected-type))))

  (define (types-error $syntax $actual-type $expected-types)
    (syntax-error $syntax
      (format "invalid type ~s, expected ~a, in"
        (type->datum $actual-type)
        (apply string-append
          (intercalate
            (map (partial format "~s")
              (map type->datum $expected-types))
            " or ")))))
)
