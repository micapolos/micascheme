(library (leo exception-handler)
  (export leo-exception-handler)
  (import
    (except (micascheme) write)
    (condition)
    (only (leo scheme) write))

  (define (leo-exception-handler $x)
    (run
      (write `(exception ,(condition->datum $x)))
      (reset)))
)
