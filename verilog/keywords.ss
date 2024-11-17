(library (verilog keywords)
  (export
    circuit wire reg
    posedge negedge always
    if range
    + and or inv
    set! ref
    append)
  (import
    (only (micascheme) define-aux-keywords))

  (define-aux-keywords
    circuit wire reg
    posedge negedge always
    if range
    + and or inv
    set! ref
    append)
)
