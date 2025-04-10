(library (emu z80 rom)
  (export define-z80-rom)
  (import (scheme) (syntax) (emu rom) (emu z80 asm))

  (define-rule-syntax (define-z80-rom (id size) body ...)
    (define-rom (id size write)
      (asm write body ...)))
)
