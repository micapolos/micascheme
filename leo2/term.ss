(library (leo2 term)
  (export
    normalized normalized? normalized-ref
    native native? native-arity native-ref
    type type? type-depth
    variable variable? variable-index
    abstraction abstraction? abstraction-param abstraction-body
    abstraction-type abstraction-type? abstraction-type-param abstraction-type-body
    application application? application-lhs application-rhs
    branch branch? branch-condition branch-motive branch-consequent branch-alternate)
  (import (leo2 base))

  (data (normalized ref))
  (data (native arity ref))
  (data (type depth))
  (data (variable index))
  (data (abstraction param body recursive?))
  (data (abstraction-type param body recursive?))
  (data (application lhs rhs))
  (data (branch condition motive consequent alternate))
)
