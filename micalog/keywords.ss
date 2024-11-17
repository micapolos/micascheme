(library (micalog keywords)
  (export
    circuit wire reg
    bit
    initial
    on positive-edge negative-edge
    if when
    + vector and or inv set! ref append)
  (import
    (only (micascheme) define-aux-keywords))

  (define-aux-keywords
    circuit wire reg
    bit
    initial
    on positive-edge negative-edge
    if when
    + vector and or inv set! ref append)
)
