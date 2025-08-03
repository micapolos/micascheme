(library (zx-next assert)
  (export
    fail
    assert
    assert-regs)
  (import
    (zx-next core)
    (zx-next throw)
    (zx-next write)
    (zx-next regs))

  (define-fragments
    (label-a (dz "A"))
    (label-b (dz "B"))
    (label-c (dz "C"))
    (label-d (dz "D"))
    (label-e (dz "E"))
    (label-h (dz "H"))
    (label-l (dz "L"))
    (label-bc (dz "BC"))
    (label-de (dz "DE"))
    (label-hl (dz "HL"))
    (label-ix (dz "IX"))
    (label-iy (dz "IY")))

  (define-fragment assert-byte
    (input (d actual) (e expected) (hl label))
    (preserve-regs
      (ld a d)
      (cp e)
      (when nz
        (preserve (de)
          (preserve (hl)
            (write-ink 4)
            (write "[ERROR] ")
            (write-ink 7)
            (write "assert "))
          (call write-string)
          (write #\space)
          (write-ink 2))
        (ld a e)
        (preserve (de)
          (call write-byte)
          (write #\space)
          (write-ink 4))
        (ld a d)
        (call write-byte)
        (write-ink 7)
        (writeln)
        (throw)))
    (ret))

  (define-ops (keywords a b c d e h l bc de hl ix iy)
    ((assert a n) (preserve (de hl) (ld d a) (ld e n) (ld hl label-a) (call assert-byte)))
    ((assert b n) (preserve (de hl) (ld d b) (ld e n) (ld hl label-b) (call assert-byte)))
    ((assert c n) (preserve (de hl) (ld d c) (ld e n) (ld hl label-c) (call assert-byte))))

  (define-asm assert-regs-expected-colors (ds 20))
  (define-asm assert-regs-actual-colors   (ds 20))

  (define-op (fail)
    (writeln-error "Failure")
    (throw))

  (define-fragment assert-regs
    (input (hl expected-regs) (de actual-regs) (bc mask-regs))
    (exx)
    (ld hl assert-regs-expected-colors)
    (ld de assert-regs-actual-colors)
    (exx)
    (preserve (hl de) (call regs-diff-colors))

    (when nz
      (preserve (hl de)
        (preserve (hl)
          (writeln-error "Registers mismatch")
          (writeln "-- Expected ---"))
        (ld de assert-regs-expected-colors)
        (call write-regs/colors))

      (preserve (hl) (writeln "-- Actual -----"))
      (ld de assert-regs-actual-colors)
      (call write-regs/colors)

      (throw))

    (ret))
)
