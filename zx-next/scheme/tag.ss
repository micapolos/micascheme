(library (zx-next scheme tag)
  (export
    constant-tag
    pair-tag)
  (import (zx-next core))

  (define-values
    (constant-tag  #b01100000)
    (pair-tag      #b11100000))
)
