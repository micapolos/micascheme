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

; Assemble something into memory
(asm mem)
(ld a #\H)
(out (#x34) a)
(ld a #\i)
(out (#x34) a)
(ld a #\!)
(out (#x34) a)
(ld a #\newline)
(out (#x34) a)
(halt)

; Define Z80
(define-z80 mem char-io z80-step z80-dump)

; Run Z80 and measure time
(time (run z80-step 35000000))

; Dump Z80 state
(z80-dump)
