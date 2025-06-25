(library (typico eval)
  (export typed-eval)
  (import (micascheme) (typico expand) (typico typed) (typico type) (typico core types))

  (define (typed-eval $typed)
    (eval
      (typed-value $typed)
      (environment '(scheme) '(typico type) '(typico core types))))
)
