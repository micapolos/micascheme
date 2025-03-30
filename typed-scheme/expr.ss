(library (typed-scheme expr)
  (export
    expr
    expr?
    expr-type
    expr-term

    lambda-term
    lambda-term?
    lambda-term-param-types
    lambda-term-body-expr

    bind-term
    bind-term?
    bind-term-bound-exprs
    bind-term-body-expr

    native-term
    native-term?
    native-term-value

    variable-term
    variable-term?
    variable-term-index

    term?)
  (import (micascheme))

  (data (expr type term))
  (data (native-term value))
  (data (lambda-term param-types body-expr))
  (data (bind-term bound-exprs body-expr))
  (data (variable-term index))

  (define (term? $obj)
    (or
      (native-term? $obj)
      (lambda-term? $obj)
      (bind-term? $obj)
      (variable-term? $obj)))
)
