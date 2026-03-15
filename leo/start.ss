(library (leo start)
  (export leo-start)
  (import
    (chezscheme)
    (leo load)
    (leo path))

  (define (leo-start)
    (load-leo (leo-path (command-line-arguments))))
)
