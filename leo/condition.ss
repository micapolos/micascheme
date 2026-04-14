(library (leo condition)
  (export print-condition)
  (import
    (scheme)
    (lets)
    (condition)
    (leo print))

  (define (print-condition $condition $port)
    (lets
      ($datum (condition->datum $condition))
      ($datum
        (if
          (and
            (pair? $datum)
            (equal? (car $datum) 'condition))
          `(error ,@(cdr $datum))
          $datum))
      (parameterize ((print-gensym #f))
        (print
          $datum
          $port))))
)
