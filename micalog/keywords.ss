(library (micalog keywords)
  (export
    module input output
    macro repeat
    on posedge negedge edge
    cond else
    register set
    wire assign
    int
    = != < <= > >=
    append slice
    + - *
    if
    and or xor nand nor xnor not)
  (import (only (micascheme) define-aux-keywords))

  (define-aux-keywords
    module input output
    macro repeat
    on posedge negedge edge
    cond else
    register set
    wire assign
    int
    = != < <= > >=
    append slice
    + - *
    if
    and or xor nand nor xnor not)
)
