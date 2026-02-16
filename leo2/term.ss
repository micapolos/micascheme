(library (leo2 term)
  (export
    hole hole?
    native native? native-arity native-value native-args
    type type? type-depth
    variable variable? variable-index
    abstraction abstraction? abstraction-param abstraction-body
    abstraction-type abstraction-type? abstraction-type-param abstraction-type-body
    application application? application-lhs application-rhs
    branch branch? branch-condition branch-motive branch-consequent branch-alternate
    recursive recursive? recursive-body)
  (import (leo2 base))

  (data hole)
  (data (native arity value args))
  (data (type depth))
  (data (variable index))
  (data (abstraction param body))
  (data (abstraction-type param body))
  (data (application lhs rhs))
  (data (branch condition motive consequent alternate))
  (data (recursive body))
)
