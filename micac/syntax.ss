(library (micac syntax)
  (export
    const var
    set
    set+ set- set* set/
    set-and set-or
    set-bitwise-and set-bitwise-ior set-bitwise-xor
    set-bitwise-arithmetic-shift-left set-bitwise-arithmetic-shift-right
    ref &ref
    while defer break-if cast then ?)
  (import (micascheme))

  (define-aux-keywords
    const var
    set
    set+ set- set* set/
    set-and set-or
    set-bitwise-and set-bitwise-ior set-bitwise-xor
    set-bitwise-arithmetic-shift-left set-bitwise-arithmetic-shift-right
    ref &ref
    while defer break-if cast then ?)
)
