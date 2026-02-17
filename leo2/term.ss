(library (leo2 term)
  (export
    hole hole?
    type type? type-depth
    boolean-type boolean-type?
    native native? native-procedure native-args
    variable variable? variable-index
    abstraction abstraction? abstraction-body
    abstraction-type abstraction-type? abstraction-type-param abstraction-type-body
    application application? application-lhs application-rhs
    branch branch? branch-condition branch-consequent branch-alternate
    recursive recursive? recursive-body
    normalized normalized? normalized-value)
  (import (leo2 base))

  (data hole)
  (data (type depth))
  (data boolean-type)
  (data (native procedure args))
  (data (variable index))
  (data (abstraction body))
  (data (abstraction-type param body))
  (data (application lhs rhs))
  (data (branch condition consequent alternate))
  (data (recursive body))
  (data (normalized value))
)
