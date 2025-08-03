(library (zx-next assert)
  (export assert-regs fail)
  (import
    (zx-next core)
    (zx-next throw)
    (zx-next write)
    (zx-next regs))

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
