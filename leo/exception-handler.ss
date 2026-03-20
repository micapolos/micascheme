(library (leo exception-handler)
  (export leo-exception-handler)
  (import
    (micascheme)
    (condition))

  (define (leo-exception-handler $x)
    (write (condition->datum $x)))
)
