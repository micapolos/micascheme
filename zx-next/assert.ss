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
    (label-iy (dz "IY"))
    (label-nc (dz "NC"))
    (label-z (dz "Z"))
    (label-nz (dz "NZ")))

  (define-op (cp-bc-de)
    (ld a b)
    (cp d)
    (when z
      (ld a c)
      (cp e)))

  (define-fragment assert-flag
    (input (fc - 0 ok / 1 failure) (hl label))
    (preserve-regs
      (when c
        (preserve (hl)
          (write-ink 4)
          (write "[ERROR] ")
          (write-ink 7)
          (write "assert ")
          (write-ink 4))
        (call write-string)
        (write-ink 7)
        (writeln)
        (throw)))
    (ret))

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

  (define-fragment assert-word
    (input (bc actual) (de expected) (hl label))
    (preserve-regs
      (cp-bc-de)
      (when nz
        (preserve (bc de)
          (preserve (hl)
            (write-ink 4)
            (write "[ERROR] ")
            (write-ink 7)
            (write "assert "))
          (call write-string)
          (write #\space)
          (write-ink 2))
        (ld h d)
        (ld l e)
        (preserve (bc de)
          (call write-word)
          (write #\space)
          (write-ink 4))
        (ld h b)
        (ld l c)
        (call write-word)
        (write-ink 7)
        (writeln)
        (throw)))
    (ret))

  (define-ops (keywords a b c d e h l bc de hl ix iy nc z nz)
    ((assert c) (preserve (af hl) (ccf) (ld hl label-c) (call assert-flag)))
    ((assert nc) (preserve (af hl) (ld hl label-nc) (call assert-flag)))
    ((assert z) (preserve (af hl) (scf) (when z (ccf)) (ld hl label-z) (call assert-flag)))
    ((assert nz) (preserve (af hl) (scf) (when nz (ccf)) (ld hl label-nz) (call assert-flag)))

    ((assert a n) (preserve (de hl) (ld d a) (ld e n) (ld hl label-a) (call assert-byte)))
    ((assert b n) (preserve (de hl) (ld d b) (ld e n) (ld hl label-b) (call assert-byte)))
    ((assert c n) (preserve (de hl) (ld d c) (ld e n) (ld hl label-c) (call assert-byte)))
    ((assert d n) (preserve (de hl) (ex de hl) (ld d h) (ld e n) (ld hl label-d) (call assert-byte)))
    ((assert e n) (preserve (de hl) (ex de hl) (ld d l) (ld e n) (ld hl label-e) (call assert-byte)))
    ((assert h n) (preserve (de hl) (ld d h) (ld e n) (ld hl label-h) (call assert-byte)))
    ((assert l n) (preserve (de hl) (ld d l) (ld e n) (ld hl label-l) (call assert-byte)))

    ((assert bc nn) (preserve (de hl) (ld de nn) (ld hl label-bc) (call assert-word)))
    ((assert de nn) (preserve (bc de hl) (ld b d) (ld c e) (ld de nn) (ld hl label-de) (call assert-word)))
    ((assert hl nn) (preserve (bc de hl) (ld b h) (ld c l) (ld de nn) (ld hl label-hl) (call assert-word)))
    ((assert ix nn) (preserve (bc de hl) (ld b ixh) (ld c ixl) (ld de nn) (ld hl label-ix) (call assert-word)))
    ((assert iy nn) (preserve (bc de hl) (ld b iyh) (ld c iyl) (ld de nn) (ld hl label-iy) (call assert-word))))

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
