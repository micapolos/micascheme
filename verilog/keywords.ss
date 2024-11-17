(library (verilog keywords)
  (export
    circuit wire reg
    bit
    positive-edge negative-edge always
    if
    + vector and or inv set! ref append)
  (import
    (only (micascheme) define-aux-keywords))

  (define-aux-keywords
    circuit wire reg
    bit
    positive-edge negative-edge always
    if
    + vector and or inv set! ref append)
)