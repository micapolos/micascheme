(library (typed-scheme expr)
  (export
    expr
    expr?
    expr-type
    expr-term

    lambda-term
    lambda-term?
    lambda-term-body

    native-term
    native-term?
    native-term-value

    variable-term
    variable-term?
    variable-term-index)
  (import (micascheme))

  (data (expr type term))
  (data (native-term value))
  (data (lambda-term body))
  (data (variable-term index))
)
