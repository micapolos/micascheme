(library (micalog keywords)
  (export
    circuit
    register bit-count initial on set if +
    wire
    positive-edge negative-edge)
  (import
    (only (micascheme) define-aux-keywords))

  (define-aux-keywords
    circuit
    register bit-count initial on set if +
    wire
    positive-edge negative-edge)
)
