(library (leo exception-handler)
  (export leo-exception-handler)
  (import
    (scheme)
    (leo condition))

  (define (leo-exception-handler $condition)
    (print-condition $condition (console-output-port))
    (reset))
)
