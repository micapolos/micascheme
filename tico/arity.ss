(library (tico arity)
  (export
    arity arity? arity-value
    arity-single?
    arity+)
  (import (micascheme))

  (data (arity value))

  (define (arity+ (arity a) (arity b))
    (arity (+ a b)))

  (define (arity-single? (arity n))
    (= n 1))
)
