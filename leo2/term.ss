(library (leo2 term)
  (export
    native native? native-ref
    native-application native-application? native-application-procedure native-application-args
    type type? type-depth
    variable variable? variable-symbol
    abstraction abstraction? abstraction-procedure
    abstraction-type abstraction-type? abstraction-type-param abstraction-type-abstraction
    application application? application-lhs application-rhs
    branch branch? branch-condition branch-consequent branch-alternate
    recursive recursive? recursive-abstraction
    evaluated evaluated? evaluated-ref)
  (import (leo2 base))

  (data (native ref))
  (data (native-application procedure args))
  (data (type depth))
  (data (variable symbol))
  (data (abstraction procedure))
  (data (abstraction-type param abstraction))
  (data (application lhs rhs))
  (data (branch condition consequent alternate))
  (data (recursive abstraction))
  (data (evaluated ref))
)
