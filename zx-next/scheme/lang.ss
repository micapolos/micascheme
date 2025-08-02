(library (zx-next scheme lang)
  (export run-scheme)
  (import
    (micascheme)
    (only (zx-next core) define-op-syntax)
    (prefix (zx-next write) %)
    (prefix (zx-next scheme primitives) %)
    (prefix (zx-next scheme compiler-keywords) %%)
    (zx-next scheme compiler))
  (export
    (import (zx-next scheme compiler-keywords)))

  (define-op-syntax run-scheme
    (lambda ($syntax)
      (lambda ($lookup)
        (syntax-case $syntax ()
          ((_ stmt ...)
            #`(begin
              (%writeln "Starting scheme...")
              (%run-scheme #,(stmt->asm $lookup #'(%%begin stmt ...)))
              (%writeln "Finishing scheme...")))))))
)
