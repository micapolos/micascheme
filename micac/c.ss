(library (micac c)
  (export micac-c)
  (import (micascheme) (micac syntax-c) (micac syntax) (syntax))
  (export (import (micac syntax)))

  (define-case-syntax (micac-c instr ...)
    (datum->syntax #'+
      (apply syntax-c (syntax->list #'(instr ...)))))
)
