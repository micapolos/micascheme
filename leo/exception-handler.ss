(library (leo exception-handler)
  (export leo-exception-handler)
  (import
    (except (micascheme) pretty-print)
    (condition)
    (only (leo scheme) pretty-print))

  (define (leo-exception-handler $x)
    (lets
      ($datum (condition->datum $x))
      ($datum (if (and (pair? $datum) (equal? (car $datum) 'condition)) (cdr $datum) (list $datum)))
        (run
          (pretty-print
            `(exception ,$datum)
            (console-error-port))
          (reset))))
)
