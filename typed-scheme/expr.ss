(library (typed-scheme expr)
  (export
    expr
    expr?
    expr-type
    expr-term

    native-term
    native-term?
    native-term-value

    bind-term
    bind-term?
    bind-term-bound-exprs
    bind-term-body-expr

    lambda-term
    lambda-term?
    lambda-term-param-types
    lambda-term-body-expr

    application-term
    application-term?
    application-term-lambda-expr
    application-term-arg-exprs

    variable-term
    variable-term?
    variable-term-index

    term?)
  (import (micascheme))

  (data (expr type term))
  (data (native-term value))
  (data (bind-term bound-exprs body-expr))
  (data (lambda-term param-types body-expr))
  (data (application-term lambda-expr arg-exprs))
  (data (variable-term index))

  (define (term? $obj)
    (or
      (native-term? $obj)
      (lambda-term? $obj)
      (bind-term? $obj)
      (variable-term? $obj)))
)
