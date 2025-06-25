(library (typico environment)
  (export typico-environment)
  (import (micascheme))

  (define delay-typico-environment
    (delay (environment '(micascheme) '(typico type) '(typico core types) '(typico scoped))))

  (define (typico-environment)
    (force delay-typico-environment))
)
