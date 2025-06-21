(import
  (only (micascheme) define-rule-syntax ...)
  (asm lang)
  (only (asm std) bytevector)
  (asm z80))

(define-rule-syntax (check-op op u8 ...)
  (check-asm
    (binary->bytevector (asm-binary (org 0) op))
    (bytevector u8 ...)))

(check-op (ret) #xc9)
