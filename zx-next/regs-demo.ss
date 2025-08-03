(import (zx-next demo) (zx-next regs))

(define-asm regs (dw #x0011 #x2233 #x4455 #x6677 #x8899 #xaabb #xccdd #xeeff #x1234 #x5678))

(demo
  (ld hl regs)
  (call write-regs)

  (ld bc #xbcbc)
  (ld a reg-bc)
  (call reg-load)
  (writeln "Loaded reg: " hl)

  (writeln)
  (writeln "Capturing regs...")
  (ld hl #x5678) (push hl)
  (ld hl #x1234) (push hl)
  (ld hl #xeeff) (push hl)
  (ld hl #xccdd) (push hl)
  (ld hl #xaabb) (push hl)
  (ld hl #x8899) (push hl)
  (ld hl #x6677) (push hl)
  (ld hl #x4455) (push hl)
  (ld hl #x2233) (push hl)
  (ld hl #x0011) (push hl)
  (pop-regs)
  (capture-regs)
  (preserve-regs
    (ld hl captured-regs)
    (call write-regs)))
