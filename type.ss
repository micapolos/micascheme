(library (type)
  (import (micascheme))

  (data (typed type value))

  (data (string-type))
  (data (boolean-type))
  (data (number-type))

  (define (typed-constant value)
    (cond
      ((boolean? value) (typed (boolean-type) value))
      ((number? value) (typed (number-type) value))
      ((string? value) (typed (string-type) value))
      (else (error "dupa"))))
)