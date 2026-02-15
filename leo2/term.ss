(library (leo2 term)
  (export
    normalized normalized? normalized-body
    native native? native-arity native-body
    type type? type-depth
    variable variable? variable-index
    abstraction-type abstraction-type? abstraction-type-recursive? abstraction-type-param abstraction-type-body
    abstraction abstraction? abstraction-recursive? abstraction-param abstraction-body
    application application? application-lhs application-rhs)
  (import (leo2 base))

  (data (normalized body))
  (data (native arity body))
  (data (type depth))
  (data (variable index))
  (data (abstraction-type recursive? param body))
  (data (abstraction recursive? param body))
  (data (application lhs rhs))
)
