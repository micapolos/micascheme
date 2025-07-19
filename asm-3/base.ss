(library (asm-3 base)
  (export define-scoped)
  (import (micascheme))
  (export
    (import
      (except (micascheme)
        environment
        environment?)))

  (define-rule-syntax (define-scoped (id scope ref))
    (data (id scope ref)))
)
