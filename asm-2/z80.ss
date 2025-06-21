(library (asm-2 z80)
  (export ret jp)
  (import
    (asm-2 lang)
    (only (micascheme) define-rule-syntax ...))

  (define-rule-syntax (define-op (id arg ...) body ...)
    (define-macro (id arg ...) (block body ...)))

  (define-op (ret) (db #xc9))
  (define-op (jp nn) (db 195) (dw nn))
)
