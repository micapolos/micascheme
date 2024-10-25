(library (micac syntax)
  (export
    u8 u16 u32
    var set add sub)
  (import (micascheme))

  (define-aux-keywords u8 u16 u32 var set add sub)
)
