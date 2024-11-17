(library (micalog keywords)
  (export
    circuit
    bit
    initial
    on positive-edge negative-edge
    if
    + vector and or not ref append)
  (import
    (only (micascheme) define-aux-keywords))

  (define-aux-keywords
    circuit
    bit
    initial
    on positive-edge negative-edge
    if
    + vector and or not ref append)
)
