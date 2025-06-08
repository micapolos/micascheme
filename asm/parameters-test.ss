(import (micascheme) (asm parameters))

(define-rule-syntax (check-parameters parameters (parameter ...))
  (check-datum=?
    (parameters->syntax parameters)
    '(parameter ...)))

(check-parameters
  (parameters-with foo bar)
  (foo bar))

(check-parameters
  (parameters-append
    (parameters-with a b c)
    (parameters-with c d e)
    (parameters-with a f g))
  (a b c d e f g))
