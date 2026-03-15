(library (leo start)
  (export leo-start leo-start-path)
  (import
    (micascheme)
    (leo load))

  (define (leo-start)
    (switch (command-line-arguments)
      ((null? _) (displayln "error: missing .leo file path in leo-start"))
      ((else $args) (leo-start-path (car $args)))))

  (define (leo-start-path $path)
    (run
      (displayln "running leo-start-path")
      (load-leo $path)))
)
