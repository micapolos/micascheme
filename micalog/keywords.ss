(library (micalog keywords)
  (export
    module input output
    macro repeat
    variable
    on posedge negedge edge
    cond else
    register set
    wire assign
    int
    = != < <= > >=
    append take drop
    wrap+ wrap- wrap*
    + - *
    if
    and or xor nand nor xnor not
    log)
  (import (only (micascheme) define-aux-keywords))

  (define-aux-keywords
    module input output
    macro repeat
    variable
    on posedge negedge edge
    cond else
    register set
    wire assign
    int
    = != < <= > >=
    append take drop
    wrap+ wrap- wrap*
    + - *
    if
    and or xor nand nor xnor not
    log)
)
