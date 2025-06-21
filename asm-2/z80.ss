(library (asm-2 z80)
  (export ret jp)
  (import
    (asm-2 lang)
    (only (micascheme) define-rule-syntax ...))

  (define-asm
    ((ret)   (db #xc9))
    ((jp nn) (db 195) (dw nn)))
)
