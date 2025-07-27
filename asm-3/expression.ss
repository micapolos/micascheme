(library (asm-3 expression)
  (export
    expression?
    pure-expression
    identifier-expression
    application-expression
    map-expressions
    map-expression
    expression->datum
    expression->syntax
    check-expression)
  (import
    (asm-3 base)
    (asm-3 dependent)
    (asm-3 relocable)
    (asm-3 org)
    (syntax lookup))

  (define-type (expression ref) (dependent (syntax ref)))

  (define (expression? $obj)
    (dependent? $obj))

  (define (pure-expression $ref)
    (pure-dependent $ref))

  (define (identifier-expression $id)
    (dependent (list $id) $id))

  (define (application-expression $fn-expression . $arg-expressions)
    (map-dependent list->syntax
      (list->dependent (cons $fn-expression $arg-expressions))))

  (define (map-expression $proc $expression)
    (map-dependent $proc $expression))

  (define (map-expressions $proc $expressions)
    (map-dependent $proc (list->dependent $expressions)))

  (define (expression->datum $expression)
    (dependent->datum
      (dependent-map $expression syntax->datum)))

  (define (expression->syntax $expression)
    (dependent->syntax $expression))

  (define-rule-syntax (check-expression expression datum)
    (check (equal? (expression->datum expression) 'datum)))
)
