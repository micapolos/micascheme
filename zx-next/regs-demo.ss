(import (zx-next demo) (zx-next regs))

(define-fragments
  (regs-2   (dw #x1234 #x5678))
  (colors-2 (dw #x0506 #x0304)))

(define-asm regs   (dw #x0011 #x2233 #x4455 #x6677 #x8899 #xaabb #xccdd #xeeff #x1234 #x5678))
(define-asm colors (dw #x0203 #x0507 #x0106 #x0204 #x0706 #x0103 #x0605 #x0304 #x0304 #x0104))

(define-asm expected-regs   (dw #x0011 #x2233 #x4455 #x6677 #x8899 #xaabb #xccdd #xeeff #x1234 #x5678))
(define-asm actual-regs     (dw #x0111 #x2233 #x4455 #x6377 #x8899 #xaabb #xcc1d #xeefd #x1234 #x5278))
(define-asm mask-regs       (dw #xffff #xffff #xffff #xffff #xffff #xffff #xffff #x0000 #x0000 #x0000))
(define-asm expected-colors (ds 20))
(define-asm actual-colors   (ds 20))

(demo
  (ld hl #x1234)
  (ld de #x0506)
  (call write-reg-word/color)

  (ld hl #x5678)
  (ld de #x0304)
  (call write-reg-word/color)
  (writeln)

  (ld hl regs-2)
  (ld de colors-2)
  (call write-ireg-word/color++)
  (call write-ireg-word/color++)
  (writeln)

  (ld hl regs)
  (ld de colors)
  (call write-regs/colors)

  (ld hl regs)
  (ld de colors)
  (call write-regs)

  (writeln "Performing diff")
  (ld hl expected-regs)
  (ld de actual-regs)
  (ld bc mask-regs)
  (exx)
  (ld hl expected-colors)
  (ld de actual-colors)
  (exx)
  (call regs-diff-colors)

  (if z
    (then
      (writeln-ok "Registers the same"))
    (else
      (writeln-error "Registers mismatch")
      (writeln "-- Expected ---")
      (ld hl expected-regs)
      (ld de expected-colors)
      (call write-regs/colors)

      (writeln "-- Actual -----")
      (ld hl actual-regs)
      (ld de actual-colors)
      (call write-regs/colors)))

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
