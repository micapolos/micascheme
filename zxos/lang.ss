(library (zxos lang)
  (export zxos)
  (import
    (micascheme)
    (only (asm run) asm-run)
    (only (asm asm-core) import-base))
  (export
    (import
      (asm lang)
      (asm z80)
      (asm run)
      (asm asm)
      (asm asm-core)))

  (define-rule-syntax (zxos body ...)
    (asm-run
      (import-base (zxos))
      body ...))
)
