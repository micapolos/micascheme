(import (scheme) (emu mem) (emu io) (emu z80 cpu) (emu z80 asm) (emu run))

; 64Kb memory
(define-mem z80-mem #x10000)

; Assemble into memory
(asm z80-mem)
(org 0)
(ld a #\H)
(out (#x34) a)
(ld a #\i)
(out (#x34) a)
(ld a #\!)
(out (#x34) a)
(ld a #\newline)
(out (#x34) a)
(halt)

; Z80 with char I/O
(define-z80 z80-mem char-io z80-step z80-dump)

; Run and measure time
(time (run z80-step 35000000))

; Dump registers
(z80-dump)
