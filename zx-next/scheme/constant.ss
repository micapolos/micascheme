(library (zx-next scheme constant)
  (export
    null-constant
    true-constant
    false-constant
    string-constant
    symbol-constant

    null-word
    true-word
    false-word)
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
    (symbol-constant (constant #x12)))

  (define-values
    (null-word  (fxsll null-constant 8))
    (false-word (fxsll false-constant 8))
    (true-word  (fxsll true-constant 8)))
)
