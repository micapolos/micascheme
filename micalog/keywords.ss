(library (micalog keywords)
  (export
    module input output
    on posedge negedge edge
    cond else
    register set
    capture
    wire assign
    int
    = != < <= > >=
    append slice
    + -
    if
    and or xor nand nor xnor not inv
    reg reg-ref)
  (import (only (micascheme) define-aux-keywords))

  (define-aux-keywords
    module input output
    on posedge negedge edge
    cond else
    register set
    capture
    wire assign
    int
    = != < <= > >=
    append slice
    + -
    if
    and or xor nand nor xnor not inv
    reg reg-ref)
)
