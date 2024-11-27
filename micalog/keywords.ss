(library (micalog keywords)
  (export
    module input output
    macro repeat
    on posedge negedge edge
    cond else
    register set set-take
    wire assign
    int
    = != < <= > >=
    append take drop
    + - *
    if
    and or xor nand nor xnor not
    log)
  (import (only (micascheme) define-aux-keywords))

  (define-aux-keywords
    module input output
    macro repeat
    on posedge negedge edge
    cond else
    register set set-take
    wire assign
    int
    = != < <= > >=
    append take drop
    + - *
    if
    and or xor nand nor xnor not
    log)
)
