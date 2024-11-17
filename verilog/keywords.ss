(library (verilog keywords)
  (export
    wire reg
    always
    * posedge negedge
    cond else range
    + -
    and or inv
    assign set! ref
    append)
  (import
    (only (micascheme) define-aux-keywords))

  (define-aux-keywords
    wire reg
    always
    * posedge negedge
    cond else range
    + -
    and or inv
    assign set! ref
    append)
)
