(library (zx-next scheme lang)
  (export run-scheme)
  (import
    (micascheme)
    (only (zx-next core) define-op-syntax)
    (prefix (zx-next write) %)
    (prefix (zx-next scheme primitives) %)
    (zx-next scheme compiler))

  (define-op-syntax run-scheme
    (lambda ($syntax)
      (lambda ($lookup)
        #`(begin
          (%writeln "Starting scheme...")
          (%run-scheme #,(expr->asm $lookup $syntax))
          (%writeln "Finishing scheme...")))))
)
