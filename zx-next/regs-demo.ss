(import (zx-next demo) (zx-next regs))

(define-asm regs (dw #x0011 #x2233 #x4455 #x6677 #x8899 #xaabb #xccdd #xeeff #x1234 #x5678))

(demo
  (ld hl regs)
  (call write-regs)

  (ld bc #xbcbc)
  (ld a reg-bc)
  (call reg-load)
  (writeln "Loaded reg: " hl))
