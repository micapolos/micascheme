(library (leo2 symbol)
  (export depth->symbol)
  (import (leo2 base))

  (define (depth->symbol $depth)
    (string->symbol (format "v~a" $depth)))
)

