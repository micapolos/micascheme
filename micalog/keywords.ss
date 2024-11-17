(library (micalog keywords)
  (export
    circuit
    register bit-count initial on write
    positive-edge negative-edge)
  (import
    (except (micascheme) write))

  (define-aux-keywords
    circuit
    register bit-count initial on write
    positive-edge negative-edge)
)
