(library (leo condition)
  (export write-condition)
  (import
    (except (micascheme) write)
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
          (cdr $datum)
          (list $datum)))
      (write
        `(exception ,$datum)
        $port)))
)
