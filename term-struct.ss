(library (term-struct)
  (export
    variable variable-number variable->datum
    )
  (import (micascheme))

  (define-one-of (term variable application))

  (define-struct (arity number))
  (define-struct (variable number))
  (define-struct (application term term-list))
  (define-struct (function arity term))

  (define (term-list->datum $list) (map term->datum $list))
)