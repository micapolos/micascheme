(import
  (only (micascheme) define-rule-syntax ...)
  (asm lang)
  (asm z80)
  (asm asm)
  (asm asm-core))

(check-asm (ret) (db #xc9))
