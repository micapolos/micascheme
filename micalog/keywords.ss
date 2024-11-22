(library (micalog keywords)
  (export
    module input output internal
    process init update
    define on cond else
    posedge negedge
    register set
    wire assign
    expr
    = != < <= > >=
    if
    append slice
    + -
    and or xor nand nor xnor not inv
    reg reg-ref)
  (import (only (micascheme) define-aux-keywords))

  (define-aux-keywords
    module input output internal
    process init update
    define on cond else
    posedge negedge
    register set
    wire assign
    expr
    = != < <= > >=
    if
    append slice
    + -
    and or xor nand nor xnor not inv
    reg reg-ref)
)
