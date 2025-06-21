(import
  (only (micascheme) define-rule-syntax ...)
  (asm-2 lang)
  (asm-2 std)
  (asm-2 z80))

(define-rule-syntax (check-op op u8 ...)
  (check-asm
    (binary->bytevector (asm-binary (org 0) op))
    (bytevector u8 ...)))

(check-op (ret) #xc9)
