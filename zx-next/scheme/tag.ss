(library (zx-next scheme tag)
  (export
    tag-mask
    byte-tag
    word-tag
    char-tag
    constant-tag
    symbol-tag
    string-tag
    pair-tag
    null-tag
    false-tag
    true-tag

    null-byte
    false-byte
    false-byte

    null-word
    false-word
    true-word)
  (import (zx-next core))

  (define-values
    (tag-mask      #b11100000)

    (byte-tag      #b00000000)
    (word-tag      #b00100000)
    (char-tag      #b01000000)
    (constant-tag  #b01100000)
    (symbol-tag    #b10000000)
    (string-tag    #b10100000)
    (pair-tag      #b11100000)

    (null-tag      #b01100000)
    (false-tag     #b01110000)
    (true-tag      #b01111000))

  (define-values
    (null-byte  #x00)
    (false-byte #x10)
    (true-byte  #x18))

  (define-values
    (null-word  (fxsll null-byte 8))
    (false-word (fxsll false-byte 8))
    (true-word  (fxsll true-byte 8)))
)
