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
    (zx-next scheme constant)
    (zx-next write))

  (define-expression (offset/byte offset byte)
    (fxior (fxsll offset 8) byte))

  (define-expression (value byte tagged-word)
    (fxior (fxsll byte 16) tagged-word))

  (define-expression (null-value)
    (value 0 (fxsll null-constant 8)))

  (define-expression (void-value)
    (value 0 (fxsll void-constant 8)))

  (define-expression (false-value)
    (value 0 (fxsll false-constant 8)))

  (define-expression (true-value)
    (value 0 (fxsll true-constant 8)))

  (define-expression (byte-value byte)
    (value byte (constant-word byte-constant)))

  (define-expression (word-value word)
    (value
      (fxand #xff word)
      (fxior (constant-word word-constant) (fxsrl word 8))))

  (define-expression (char-value char)
    (value char (constant-word char-constant)))

  (define-expression (string-value address)
    (value
      (fxand #xff address)
      (fxior (fxsll string-constant 8) (fxsrl address 8))))

  (define-expression (symbol-value  address)
    (value
      (fxand #xff address)
      (fxior
        (fxsll symbol-constant 8)
        (fxsrl address 8))))

  (define-expression (pair-value address)
    (value 0 (tagged-word pair-tag (fxand #x1fff address))))

  (define-expression (procedure-value address)
    (value
      (fxand address #xff)
      (fxior (fxsll procedure-tag 8) (fxand address #x1fff))))
)
