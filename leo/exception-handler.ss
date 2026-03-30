(library (leo exception-handler)
  (export leo-exception-handler)
  (import
    (micascheme)
    (leo condition))

  (define (leo-exception-handler $condition)
    (run
      (write-condition $condition (console-output-port))
      (reset)))
)
