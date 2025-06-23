(library (typico expand)
  (export
    typed typed? typed-type typed-value
    primitive-type primitive-type?
    function-type function-type? function-type-param-types function-type-result-type
    boolean-type
    integer-type
    expand-typed
    default-expand-typed
    type-assignable?)
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
        (typed boolean-type $datum/annotation))
      ((number? _)
        (typed integer-type $datum/annotation))
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
                ($stripped
                  `(
                    ,(datum/annotation-stripped (typed-value $typed-car))
                    ,@(map datum/annotation-stripped (map typed-value $typed-args))))
                (typed
                  (function-type-result-type $function-type)
                  (switch $datum/annotation
                    ((annotation? $annotation)
                      (make-annotation
                        `(
                          ,(datum/annotation-expression (typed-value $typed-car))
                          ,@(map datum/annotation-expression (map typed-value $typed-args)))
                        (annotation-source $annotation)
                        $stripped
                        (annotation-option-set $annotation)))
                    ((else _) $stripped)))))
            ((else $other-type)
              (syntax-error $car "not a function")))))))

  (define (type-assignable? $type $from-type)
    (equal? $type $from-type))
)
