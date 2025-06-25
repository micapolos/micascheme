(library (typico environment)
  (export typico-environment)
  (import (micascheme))

  (define (typico-environment)
    (environment '(micascheme) '(typico type) '(typico core types) '(typico scoped)))
)
