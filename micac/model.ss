(library (micac model)
  (export
    type type? type-identifier type-size
    variable variable? variable-identifier variable-type variable-offset
    body body? body-variables body-syntaxes body-size
    expr expr? expr-type expr-syntax

    type->datum
    variable->datum
    body->datum)
  (import (micascheme))

  (data (type identifier size))

  (data (variable identifier type offset))

  (data (body variables syntaxes size))

  (data (expr type syntax))

  (define (type->datum $type)
    `(type
      ,(syntax->datum (type-identifier $type))
      ,(type-size $type)))

  (define (variable->datum $variable)
    `(variable
      ,(syntax->datum (variable-identifier $variable))
      ,(type->datum (variable-type $variable))
      ,(variable-offset $variable)))

  (define (body->datum $body)
    `(body
      ,(reverse (map variable->datum (body-variables $body)))
      ,(reverse (map syntax->datum (body-syntaxes $body)))
      ,(body-size $body)))

  (define (expr->datum $expr)
    `(expr
      ,(type->datum (expr-type $expr))
      ,(syntax->datum (expr-syntax $expr))))
)
