(library (micalog keywords)
  (export
    register bit-count init on write
    positive-edge negative-edge)
  (import
    (except (micascheme) write))

  (define-aux-keywords
    register bit-count init on write
    positive-edge negative-edge)
)
