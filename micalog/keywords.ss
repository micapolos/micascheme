(library (micalog keywords)
  (export
    micalog
    module input output internal
    define on cond else
    edge-01 edge-10
    register
    expr
    append slice
    + - add sub
    and or not inv
    reg reg-ref)
  (import (only (micascheme) define-aux-keywords))

  (define-aux-keywords
    micalog
    module input output internal
    define on cond else
    edge-01 edge-10
    register
    expr
    append slice
    + - add sub
    and or not inv
    reg reg-ref)
)
