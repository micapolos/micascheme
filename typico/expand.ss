(library (typico expand)
  (export
    boolean-type
    integer-type
    expand-typed
    default-expand-typed
    typed-value-of)
  (import (micascheme) (typico type) (typico typed) (typico core-types))

  (define (expand-typed $lookup $syntax)
    (switch (syntax-selector $syntax)
      ((false? _) (default-expand-typed $lookup $syntax))
      ((else $id) (($lookup $id) $lookup $syntax))))

  (define (default-expand-typed $lookup $syntax)
    (syntax-case $syntax ()
      (b
        (boolean? (datum b))
        (typed boolean-type (datum b)))
      (i
        (integer? (datum i))
        (typed integer-type (datum i)))
      ((fn arg ...)
        (lets
          ($typed-fn (expand-typed $lookup #'fn))
          (switch (typed-type $typed-fn)
            ((function-type? $function-type)
              (lets
                ($typed-args (map (partial expand-typed $lookup) #'(arg ...)))
                (typed
                  (function-type-result-type $function-type)
                  `(
                    ,(typed-value $typed-fn)
                    ,@(typed-arg-values
                      $syntax
                      (function-type-param-types $function-type)
                      $typed-args
                      #'(arg ...))))))
            ((else $other-type)
              (syntax-error #'fn "not a function")))))))

  (define (typed-value-of $syntax $type $typed)
    (cond
      ((equal? $type (typed-type $typed))
        (typed-value $typed))
      (else
        (syntax-error $syntax "invalid type"))))

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
)
