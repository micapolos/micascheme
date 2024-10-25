(library (micac c)
  (export micac-c)
  (import (micascheme) (micac syntax-c) (micac syntax) (syntax))
  (export (import (micac syntax)))

  (define-case-syntax (micac-c instr ...)
    (datum->syntax #'+
      (syntax-c #`(begin instr ...))))
)
