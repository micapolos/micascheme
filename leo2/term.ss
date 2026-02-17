(library (leo2 term)
  (export
    native native? native-value
    native-application native-application? native-application-procedure native-application-args
    type type? type-depth
    variable variable? variable-index
    abstraction abstraction? abstraction-body
    abstraction-type abstraction-type? abstraction-type-param abstraction-type-body
    application application? application-lhs application-rhs
    branch branch? branch-condition branch-consequent branch-alternate
    recursive recursive? recursive-body
    hole hole?
    normalized normalized? normalized-value
    closure closure? closure-env closure-body)
  (import (leo2 base))

  (data (native value))
  (data (native-application procedure args))
  (data (type depth))
  (data (variable index))
  (data (abstraction body))
  (data (abstraction-type param body))
  (data (application lhs rhs))
  (data (branch condition consequent alternate))
  (data (recursive body))

  (data hole)
  (data (normalized value))
  (data (closure env body))
)
