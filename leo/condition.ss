(library (leo condition)
  (export write-condition)
  (import
    (except (scheme) write)
    (lets)
    (condition)
    (leo write))

  (define (write-condition $condition $port)
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
        (write
          $datum
          $port))))
)
