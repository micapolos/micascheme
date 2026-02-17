(library (leo2 term)
  (export
    type type? type-depth
    native-application native-application? native-application-procedure native-application-args
    variable variable? variable-index
    abstraction abstraction? abstraction-body
    abstraction-type abstraction-type? abstraction-type-param abstraction-type-body
    application application? application-lhs application-rhs
    branch branch? branch-condition branch-consequent branch-alternate
    recursive recursive? recursive-body)
  (import (leo2 base))

  (data (type depth))
  (data (native-application procedure args))
  (data (variable index))
  (data (abstraction body))
  (data (abstraction-type param body))
  (data (application lhs rhs))
  (data (branch condition consequent alternate))
  (data (recursive body))
)
