(library (micalog keywords)
  (export
    circuit
    register bit-count initial on write when
    wire
    positive-edge negative-edge)
  (import
    (except (micascheme) write when))

  (define-aux-keywords
    circuit
    register bit-count initial on write when
    wire
    positive-edge negative-edge)
)
