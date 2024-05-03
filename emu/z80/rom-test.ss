(import (scheme) (check) (emu z80 rom) (emu z80 asm))

(let ()
  (define-z80-rom (rom 4)
    (ld a 10)
    (halt))

  (check (equal? (rom 0) 62))
  (check (equal? (rom 1) 10))
  (check (equal? (rom 2) 118))
  (check (equal? (rom 3) 0)))
