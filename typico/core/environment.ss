(library (typico core environment)
  (export typico-environment)
  (import (micascheme))

  (define delay-typico-environment
    (delay
      (environment
        '(micascheme)
        '(typico type)
        '(typico core types)
        '(typico core dynamic))))

  (define (typico-environment)
    (force delay-typico-environment))
)
