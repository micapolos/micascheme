(library (asm z80-cspect)
  (export break exit)
  (import
    (asm lang)
    (asm z80))
  (export (import (asm z80)))

  (define-ops
    ((break)           (db #xfd #x00))
    ((exit)            (db #xdd #x00)))
)
