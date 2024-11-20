(library (micalog keywords)
  (export
    module input output internal
    process init update
    define on cond else
    posedge negedge
    register
    expr
    = != < <= > >=
    if
    append slice
    + - add sub neg
    and or xor nand nor xnor not inv
    reg reg-ref)
  (import (only (micascheme) define-aux-keywords))

  (define-aux-keywords
    module input output internal
    process init update
    define on cond else
    posedge negedge
    register
    expr
    = != < <= > >=
    if
    append slice
    + - add sub neg
    and or xor nand nor xnor not inv
    reg reg-ref)
)
