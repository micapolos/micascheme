(library (zx-next scheme constant)
  (export
    null-constant
    true-constant
    false-constant
    string-constant
    symbol-constant
    char-constant

    null-constant-word
    true-constant-word
    false-constant-word)
  (import
    (zx-next core)
    (zx-next tag)
    (zx-next tagged)
    (zx-next scheme tag))

  (define-expression (constant n)
    (tagged-byte constant-tag n))

  (define-values
    (null-constant   (constant #x00))
    (true-constant   (constant #x10))
    (false-constant  (constant #x18))
    (string-constant (constant #x11))
    (symbol-constant (constant #x12))
    (char-constant   (constant #x13)))

  (define-values
    (null-constant-word  (fxsll null-constant 8))
    (false-constant-word (fxsll false-constant 8))
    (true-constant-word  (fxsll true-constant 8)))
)
