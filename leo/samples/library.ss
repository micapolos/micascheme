(library (leo term)
  (export
    native native? native-ref
    application application? application-lhs application-rhs
    abstraction abstraction? abstraction-procedure)
  (import
    (leo base)
    (leo datum))

  (data (native ref))
  (data (application lhs rhs))
  (data (abstraction procedure))
)
