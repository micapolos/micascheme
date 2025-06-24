(library (typico expand)
  (export
    typed typed? typed-type typed-value
    primitive-type primitive-type?
    function-type function-type? function-type-param-types function-type-result-type
    boolean-type
    integer-type
    expand-typed
    default-expand-typed
    typed-value-of)
  (import (micascheme))

  (data (typed type value))

  (data (primitive-type gensym datum))
  (data (function-type param-types result-type))

  (define boolean-type (primitive-type (gensym) 'boolean))
  (define integer-type (primitive-type (gensym) 'integer))

  (define (expand-typed $lookup $datum/annotation)
    (switch (datum/annotation-expression $datum/annotation)
      ((symbol? _)
        (($lookup $datum/annotation) $lookup $datum/annotation))
      ((pair? (pair $car $cdr))
        (switch (datum/annotation-expression $car)
          ((symbol? _)
            (($lookup $car) $lookup $datum/annotation))
          ((else _)
            (default-expand-typed $lookup $datum/annotation))))
      ((else _)
        (default-expand-typed $lookup $datum/annotation))))

  (define (default-expand-typed $lookup $datum/annotation)
    (switch (datum/annotation-expression $datum/annotation)
      ((boolean? _)
        (typed boolean-type (datum/annotation-stripped $datum/annotation)))
      ((number? _)
        (typed integer-type (datum/annotation-stripped $datum/annotation)))
      ((pair? (pair $car $cdr))
        (lets
          ($typed-car (expand-typed $lookup $car))
          (switch (typed-type $typed-car)
            ((function-type? $function-type)
              (lets
                ($typed-args
                  (map*
                    (partial expand-typed $lookup)
                    (lambda (_) (syntax-error $cdr "not a proper list"))
                    (datum/annotation-expression $cdr)))
                (typed
                  (function-type-result-type $function-type)
                  `(
                    ,(datum/annotation-stripped (typed-value $typed-car))
                    ,@(typed-arg-values
                      $datum/annotation
                      (function-type-param-types $function-type)
                      $typed-args
                      (datum/annotation-expression $cdr))))))
            ((else $other-type)
              (syntax-error $car "not a function")))))))

  (define (typed-value-of $datum/annotation $type $typed)
    (cond
      ((equal? $type (typed-type $typed))
        (typed-value $typed))
      (else
        (syntax-error $datum/annotation "invalid type"))))

  (define (typed-arg-values $datum/annotation $param-types $typed-args $arg-datum/annotations)
    (switch $param-types
      ((null? _)
        (cond
          ((null? $typed-args) '())
          (else (syntax-error $datum/annotation "illegal argument count"))))
      ((pair? (pair $param-type $param-types))
        (switch $typed-args
          ((pair? (pair $typed-arg $typed-args))
            (cons
              (typed-value-of (car $arg-datum/annotations) $param-type $typed-arg)
              (typed-arg-values $datum/annotation $param-types $typed-args (cdr $arg-datum/annotations))))
          ((else _)
            (syntax-error $datum/annotation "illegal argument count"))))
      ((else $vararg-type)
        (map (partial typed-value-of $datum/annotation $vararg-type) $typed-args))))
)
