(library (zx-next scheme constant)
  (export
    byte-constant
    word-constant
    null-constant
    void-constant
    true-constant
    false-constant
    string-constant
    symbol-constant
    char-constant

    constant-word)
  (import
    (zx-next core)
    (zx-next tag)
    (zx-next tagged)
    (zx-next scheme tag))

  (define-expression (constant n)
    (tagged-byte constant-tag n))

  (define-values
    (null-constant   (constant #x00))
    (void-constant   (constant #x01))
    (byte-constant   (constant #x02))
    (word-constant   (constant #x03))
    (true-constant   (constant #x04))
    (false-constant  (constant #x05))
    (symbol-constant (constant #x06))
    (string-constant (constant #x07))
    (char-constant   (constant #x08)))

  (define-expression (constant-word constant)
    (fxsll constant 8))
)
