(library (zx-next scheme tag)
  (export
    procedure-tag
    constant-tag
    pair-tag)
  (import (zx-next core))

  (define-values
    (procedure-tag #b00100000)
    (constant-tag  #b01100000)
    (pair-tag      #b11100000))
)
