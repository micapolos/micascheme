(library (leo start)
  (export leo-start)
  (import
    (micascheme)
    (leo load))

  (define (leo-start)
    (switch (command-line-arguments)
      ((null? _)
        (displayln "error: missing .leo file path"))
      ((else $args)
        (load-leo (car $args)))))
)
