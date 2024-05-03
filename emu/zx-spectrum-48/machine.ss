(library (emu zx-spectrum-48 machine)
  (export define-zx-spectrum-48)
  (import (scheme) (syntax) (emu mem) (emu io) (emu z80 cpu) (emu z80 asm))

  (define-rule-syntax (define-zx-spectrum-48 id init step)
    (begin
      (define-mem mem #x10000)

      (define-z80 mem char-io z80-step)

      (define-rule-syntax (init)
        (asm mem
          (ld a #\H)
          (out (#x34) a)
          (ld a #\i)
          (out (#x34) a)
          (ld a #\!)
          (out (#x34) a)
          (ld a #\newline)
          (out (#x34) a)
          (halt)))

      (define-rule-syntax (step)
        (z80-step))))
)
