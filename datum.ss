(library (datum)
  (export pretty-datum)
  (import (scheme) (switch) (pair))

  (define (pretty-datum $datum)
    (switch $datum
      ((gensym? $gensym)
        (string->symbol (symbol->string $gensym)))
      ((pair? (pair $car $cdr))
        (cons
          (pretty-datum $car)
          (pretty-datum $cdr)))
      ((else $other) $other)))
)
