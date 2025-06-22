(library (zxos)
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
      (asm asm-core)
      (asm std)
      (zxos mmu)))

  (define-rule-syntax (zxos body ...)
    (asm-run
      (import-base (zxos))
      body ...))
)
