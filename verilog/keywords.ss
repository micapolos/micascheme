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
    append)
  (import (only (micascheme) define-aux-keywords))

  (define-aux-keywords
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
    append)
)
