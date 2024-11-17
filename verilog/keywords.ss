(library (verilog keywords)
  (export
    wire reg
    posedge negedge always
    cond else range
    + - * and or inv
    assign set! ref
    append)
  (import
    (only (micascheme) define-aux-keywords))

  (define-aux-keywords
    wire reg
    posedge negedge always
    cond else range
    + - * and or inv
    assign set! ref
    append)
)
