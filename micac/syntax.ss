(library (micac syntax)
  (export
    micac
    u8 u16 u32
    var set add sub
    shl shr
    while)
  (import (micascheme))

  (define-aux-keywords micac u8 u16 u32 var set add sub shl shr while)
)
