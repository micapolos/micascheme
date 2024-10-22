(library (micac syntax)
  (export
    bool u8 u16 u32
    var)
  (import (micascheme))

  (define-aux-keywords bool u8 u16 u32)
  (define-aux-keywords var)
)
