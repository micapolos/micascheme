(library (leo2 term)
  (export
    normalized normalized? normalized-body
    native native? native-arity native-body
    type type? type-depth
    variable variable? variable-index
    abstraction abstraction? abstraction-param abstraction-body
    abstraction-type abstraction-type? abstraction-type-param abstraction-type-body
    application application? application-lhs application-rhs
    injection-type injection-type? injection-type-lhs injection-type-rhs
    injection injection? injection-lhs? injection-body
    decision decision? decision-target decision-motive decision-lhs decision-rhs)
  (import (leo2 base))

  (data (normalized body))
  (data (native arity body))
  (data (type depth))
  (data (variable index))
  (data (abstraction-type param body recursive?))
  (data (abstraction param body recursive?))
  (data (application lhs rhs))
  (data (injection-type lhs rhs))
  (data (injection lhs? body))
  (data (decision target motive lhs rhs))
)
