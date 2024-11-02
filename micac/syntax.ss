(library (micac syntax)
  (export
    micac
    u8 u16 u32
    !=
    var set ref &ref
    add sub
    shl shr
    while defer break-if)
  (import (micascheme))

  (define-aux-keywords micac != u8 u16 u32 var set ref &ref add sub shl shr while defer break-if)
)
