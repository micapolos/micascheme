(library (micac lang)
  (export micac run externs macro)
  (import
    (scheme)
    (syntax)
    (syntaxes)
    (micac syntax)
    (micac c)
    (micac run))
  (export (import (micac syntax)))
  (export (import (only (scheme)
    = < <= > >= + - * /
    begin if
    not
    and or bitwise-and bitwise-ior bitwise-xor)))

  (define-aux-keywords run externs macro)

  (define-rules-syntax (literals run externs macro)
    ((micac) (begin))
    ((micac (run body ...))
      (micac-run body ...))
    ((micac (externs body ...))
      (micac-externs body ...))
    ((micac (macro body ...))
      (micac-macro body ...))
    ((micac x xs ...)
      (begin
        (micac x)
        (micac xs ...))))
)
