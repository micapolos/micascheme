(library (micac syntax)
  (export
    micac
    !=
    var set ref &ref
    add sub
    shl shr
    while defer break-if)
  (import (micascheme))

  (define-aux-keywords micac != var set ref &ref add sub shl shr while defer break-if)
)
