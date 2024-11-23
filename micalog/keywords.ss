(library (micalog keywords)
  (export
    module input output
    on posedge negedge
    cond else
    register set
    wire assign
    int
    = != < <= > >=
    append slice
    + -
    and or xor nand nor xnor not inv
    reg reg-ref)
  (import (only (micascheme) define-aux-keywords))

  (define-aux-keywords
    module input output
    on posedge negedge
    cond else
    register set
    wire assign
    int
    = != < <= > >=
    append slice
    + -
    and or xor nand nor xnor not inv
    reg reg-ref)
)
