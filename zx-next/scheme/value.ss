(library (zx-next scheme value)
  (export
    offset/byte
    value

    null-value
    void-value
    false-value
    true-value
    byte-value
    word-value
    char-value
    string-value
    symbol-value
    pair-value
    procedure-value)
  (import
    (zx-next core)
    (zx-next tag)
    (zx-next tagged)
    (zx-next scheme tag)
    (zx-next scheme constant))

  (define-expression (offset/byte offset byte)
    (fxior (fxsll offset 8) byte))

  (define-expression (value offset byte tagged-word)
    (fxior
      (fxsll offset 24)
      (fxsll byte 16)
      tagged-word))

  (define-expression (null-value offset)
    (value
      offset
      0
      (fxsll null-constant 8)))

  (define-expression (void-value offset)
    (value
      offset
      0
      (fxsll void-constant 8)))

  (define-expression (false-value offset)
    (value
      offset
      0
      (fxsll false-constant 8)))

  (define-expression (true-value offset)
    (value
      offset
      0
      (fxsll true-constant 8)))

  (define-expression (byte-value offset byte)
    (value
      offset
      byte
      byte-constant-word))

  (define-expression (word-value offset word)
    (value
      offset
      (fxand #xff word)
      (fxior word-constant-word (fxsrl word 8))))

  (define-expression (char-value offset char)
    (value
      offset
      char
      char-constant-word))

  (define-expression (string-value offset address)
    (value
      offset
      (fxand #xff address)
      (fxior (fxsll string-constant 8) (fxsrl address 8))))

  (define-expression (symbol-value offset address)
    (value
      offset
      (fxand #xff address)
      (fxior (fxsll symbol-constant 8) (fxsrl address 8))))

  (define-expression (pair-value offset address)
    (value
      offset
      0
      (tagged-word pair-tag (fxand #x1fff address))))

  (define-expression (procedure-value offset address)
    (value
      offset
      (fxand address #xff)
      (fxior (fxsll procedure-tag 8) (fxand address #x1fff)))))
