(library (literal)
  (export literal?)
  (import (scheme))

  (define (literal? $obj)
    (or
      (boolean? $obj)
      (char? $obj)
      (number? $obj)
      (string? $obj)
      (symbol? $obj)))
)
