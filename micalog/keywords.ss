(library (micalog keywords)
  (export
    module input output
    macro
    on posedge negedge edge
    cond else
    register set
    wire assign
    int
    = != < <= > >=
    append slice
    + -
    if
    inc dec
    set+ set-
    and or xor nand nor xnor not inv
    reg reg-ref)
  (import (only (micascheme) define-aux-keywords))

  (define-aux-keywords
    module input output
    macro
    on posedge negedge edge
    cond else
    register set
    wire assign
    int
    = != < <= > >=
    append slice
    + -
    if
    inc dec
    set+ set-
    and or xor nand nor xnor not inv
    reg reg-ref)
)
