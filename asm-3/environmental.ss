(library (asm-3 environmental)
  (export
    environmental environmental? environmental-environment environmental-ref
    environmental-with-environment environmental-with-ref
    pure-environmental
    environmental-map
    environmental-update-environment
    environmental-append
    list->environmental)
  (import (asm-3 base) (asm-3 environment))

  (define-monoidical (environmental environment))
)
