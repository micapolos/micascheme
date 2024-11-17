(library (verilog keywords)
  (export
    circuit wire reg
    posedge negedge always
    when cond else range
    + - * and or inv
    assign set! ref
    append)
  (import
    (only (micascheme) define-aux-keywords))

  (define-aux-keywords
    circuit wire reg
    posedge negedge always
    when cond else range
    + - * and or inv
    assign set! ref
    append)
)
