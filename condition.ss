(library (condition)
  (export condition->datum)
  (import
    (scheme)
    (switch))

  (define (syntax->condition-datum $syntax)
    (syntax->datum $syntax))

  (define (condition->datum $condition)
    (switch $condition
      ((syntax-violation? $syntax-violation)
        `(syntax-violation
          ,(syntax->condition-datum (syntax-violation-form $syntax-violation))
          ,(syntax->condition-datum (syntax-violation-subform $syntax-violation))))
      ((else $other)
        $other)))
)
