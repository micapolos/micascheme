(import
  (scheme)
  (emu mem)
  (emu io)
  (emu z80)
  (emu run)
  (emu asm)
  (syntaxes)
  (system))

; 64Kb memory
(define-mem mem #x10000)

; Assemble into memory
(asm mem)
(org 0)
(nop)
(nop)
(ld a (char->integer #\a))
(out (#x12) a)
(ld b a)
(ld c a)
(ld (#x1000) a)
(jp 0)

; Define Z80
(define-z80 mem char-io z80-step z80-dump)

; Run Z80 and measure time
(time (run z80-step 35000000))

; Dump Z80 state
(z80-dump)
