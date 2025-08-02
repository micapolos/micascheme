(library (zx-next scheme lang)
  (export run-scheme)
  (import
    (micascheme)
    (only (zx-next core) define-op-syntax)
    (zx-next scheme compiler))

  (define-op-syntax run-scheme
    (lambda ($syntax)
      (lambda ($lookup)
        (expr->asm $lookup $syntax))))
)
