(library (asm-2 expression)
  (export
    expression expression? expression-org->value-proc
    expression-with
    expression->value
    expression-map)
  (import (micascheme))

  (data (expression org->value-proc))

  (define-rule-syntax (expression-with ($org) body)
    (expression (lambda ($org) body)))

  (define (expression->value $expression $org)
    ((expression-org->value-proc $expression) $org))

  (define (expression-map $proc $expression)
    (expression
      (lambda ($org)
        ($proc (expression->value $expression $org)))))
)
