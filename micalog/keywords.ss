(library (micalog keywords)
  (export
    micalog
    module input output internal
    define on cond else
    edge-01 edge-10
    register
    expr
    init
    = != < <= > >=
    append slice
    + - add sub neg
    and or xor nand nor xnor not inv
    reg reg-ref)
  (import (only (micascheme) define-aux-keywords))

  (define-aux-keywords
    micalog
    module input output internal
    define on cond else
    edge-01 edge-10
    register
    expr
    init
    = != < <= > >=
    append slice
    + - add sub neg
    and or xor nand nor xnor not inv
    reg reg-ref)
)
