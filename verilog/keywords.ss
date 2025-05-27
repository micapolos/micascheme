(library (verilog keywords)
  (export
    module input output
    wire reg
    always
    * posedge negedge
    if cond else to
    + -
    not
    and or xor
    nand nor xnor
    < <= > >= = !=
    assign set! ref
    append
    display)
  (import (only (micascheme) define-keywords))

  (define-keywords
    module input output
    wire reg
    always
    * posedge negedge
    if cond else to
    + -
    not
    and or xor
    nand nor xnor
    < <= > >= = !=
    assign set! ref
    append
    display)
)
