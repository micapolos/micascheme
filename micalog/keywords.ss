(library (micalog keywords)
  (export
    circuit
    register size initial on set if +
    wire
    positive-edge negative-edge)
  (import
    (only (micascheme) define-aux-keywords))

  (define-aux-keywords
    circuit
    register size initial on set if +
    wire
    positive-edge negative-edge)
)
