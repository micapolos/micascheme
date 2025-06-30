(library (typico core dynamic)
  (export dynamic)
  (import (micascheme))

  (define-rule-syntax (dynamic x) x)
)
