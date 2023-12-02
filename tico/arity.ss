(library (tico arity)
  (export
    arity arity? arity-value
    arity-single?
    arity+)
  (import (micascheme))

  (data (arity value))

  (function (arity+ (arity a) (arity b))
    (arity (+ a b)))

  (function (arity-single? (arity n))
    (= n 1))
)
