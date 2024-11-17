(library (micalog keywords)
  (export
    circuit
    register bit-count init on write
    positive-edge negative-edge)
  (import
    (except (micascheme) write))

  (define-aux-keywords
    circuit
    register bit-count init on write
    positive-edge negative-edge)
)
