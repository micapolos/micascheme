(library (typico expand)
  (export
    boolean-type
    integer-type
    expand-typed
    expand-typed/no-lookup
    expand-value-of)
  (import
    (micascheme)
    (typico type)
    (typico core-types)
    (typico typed))

  (define (expand-typed $lookup $syntax)
    (switch (syntax-selector $syntax)
      ((false? _)
        (expand-typed/no-lookup $lookup $syntax))
      ((else $id)
        (switch ($lookup (syntax->datum $id))
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
      (n
        (number? (datum n))
        (typed number-type (datum n)))
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
        (syntax-error $syntax "invalid type"))))

  (define (expand-typed-function $lookup $syntax)
    (lets
      ($typed (expand-typed $lookup $syntax))
      (switch (typed-type $typed)
        ((function-type? $function-type) $typed)
        ((else $other-type) (syntax-error #'fn "not a function")))))

  (define (expand-value-of $lookup $type $syntax)
    (lets
      ($typed (expand-typed $lookup $syntax))
      (cond
        ((type=? $type (typed-type $typed))
          (typed-value $typed))
        (else
          (type-error (typed-type $typed) $type $syntax)))))

  (define (type-error $actual-type $expected-type $syntax)
    (syntax-error $syntax
      (format "invalid type ~s, expected ~s, in"
        (type->datum $actual-type)
        (type->datum $expected-type))))
)
