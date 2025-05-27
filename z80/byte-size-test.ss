(import (micascheme) (z80 prim) (z80 byte-size))

(check (equal? (byte-size u8) 1))
(check (equal? (byte-size u16) 2))
(check (equal? (byte-size (* 8 u16)) 16))
(check (equal? (byte-size (vector u8 u16 (* 3 u8))) 6))
