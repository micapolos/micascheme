(library (typico eval)
  (export typed-eval)
  (import (micascheme) (typico expand) (typico typed) (typico type) (typico core types) (typico environment))

  (define (typed-eval $typed)
    (eval
      (typed-value $typed)
      (typico-environment)))
)
