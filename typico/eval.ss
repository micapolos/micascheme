(library (typico eval)
  (export typed-eval)
  (import (micascheme) (typico expand) (typico typed) (typico type) (typico core types))

  (define (typed-eval $typed)
    (lets
      ((typed $type $value) $typed)
      (cond
        ((type=? $type type-type) $value)
        (else
          (eval
            (typed-value $typed)
            (environment '(scheme)))))))
)
