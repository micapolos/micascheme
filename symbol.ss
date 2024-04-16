(library (symbol)
  (export symbol-append)
  (import (scheme))

  (define (symbol-append . $symbols)
    (string->symbol
      (apply string-append
        (map symbol->string $symbols))))
)
