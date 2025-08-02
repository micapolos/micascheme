(library (zx-next scheme value)
  (export
    byte-tag
    word-tag
    char-tag
    constant-tag
    symbol-tag
    string-tag
    null-tag
    false-tag
    true-tag)
  (import (zx-next core))

  (define-values
    (byte-tag      #b00000000)
    (word-tag      #b00100000)
    (char-tag      #b01000000)
    (constant-tag  #b01100000)
    (symbol-tag    #b10000000)
    (string-tag    #b10100000)

    (null-tag      #b01100000)
    (false-tag     #b01110000)
    (true-tag      #b01111000))
)
