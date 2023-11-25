(library (tico arity)
  (export arity arity? arity-value
    arity+)
  (import (micascheme))

  (data (arity value))

  (define (arity+ $arity-a $arity-b)
    (arity
      (+
        (arity-value $arity-a)
        (arity-value $arity-b))))
)
