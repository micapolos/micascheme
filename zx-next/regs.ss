(library (zx-next regs)
  (export
    reg-af
    reg-hl
    reg-bc
    reg-de
    reg-af2
    reg-hl2
    reg-bc2
    reg-de2
    reg-ix
    reg-iy

    reg-load
    regs-size
    write-regs

    capture-regs
    captured-regs

    write-reg-byte/color
    write-reg-word/color
    write-regs/colors
    write-ireg-word/color++

    regs-diff-colors)
  (import
    (zx-next core)
    (zx-next write)
    (zx-next dispatch))

  (define-values
    (reg-af  #x00)
    (reg-hl  #x01)
    (reg-bc  #x02)
    (reg-de  #x03)
    (reg-af2 #x04)
    (reg-hl2 #x05)
    (reg-bc2 #x06)
    (reg-de2 #x07)
    (reg-ix  #x08)
    (reg-iy  #x09))

  (define-asm reg-load
    (input (a reg))
    (output (hl value))
    (ret-dispatch
      (begin load-af (push af) (pop hl))
      (begin load-hl)
      (begin load-bc (ld h b) (ld l c))
      (begin load-de (ld h d) (ld l e))
      (begin load-af2 (exx) (push af) (pop hl))
      (begin load-hl2 (exx))
      (begin load-bc2 (exx) (ld h b) (ld l c))
      (begin load-de2 (exx) (ld h d) (ld l e))
      (begin load-ix (push ix) (pop hl))
      (begin load-iy (push iy) (pop hl))))

  (define-value    reg-name-size 3)
  (define-fragment reg-names (ascii "AF HL BC DE AF'HL'BC'DE'IX IY "))

  (define-value regs-size 20)
  (define-fragment captured-regs (ds 20))

  (define-op (capture-regs)
    (push-regs)
    (ld de captured-regs)
    (ld hl 0)
    (add hl sp)
    (ld bc regs-size)
    (ldir)
    (pop-regs))

  (define-asm write-reg-byte/color
    (input (a reg) (l color))
    (preserve (af)
      (preserve (hl)
        (ld a #x10)
        (call write-char))
      (ld a l)
      (call write-char))
    (call write-byte)
    (ld a #x10)
    (call write-char)
    (ld a #x07)
    (jp write-char))

  (define-asm write-reg-word/color
    (input (hl reg) (de color))
    (preserve (hl de)
      (ld a h)
      (ld l d)
      (call write-reg-byte/color))
    (ld a l)
    (ld l e)
    (jp write-reg-byte/color))

  (define-asm write-ireg-word/color++
    (input (hl reg ptr) (de color ptr))
    (ld c (hl))
    (inc hl)
    (ld b (hl))
    (inc hl)
    (preserve (hl)
      (ex de hl)
      (ld e (hl))
      (inc hl)
      (ld d (hl))
      (inc hl)
      (ex de hl)
      (preserve (de)
        (ld d h)
        (ld e l)
        (ld h b)
        (ld l c)
        (call write-reg-word/color)))
    (ret))

  (define-fragment regs-default-colors (ds 20 #x07))

  (define-asm write-regs
    (input (hl regs))
    (ld de regs-default-colors)
    (jp write-regs/colors))

  (define-asm write-regs/colors
    (input (hl regs) (de colors))

    (preserve (hl de) (write "AF "))
    (call write-ireg-word/color++)
    (add hl 6)
    (add de 6)

    (preserve (hl de) (write " AF'"))
    (call write-ireg-word/color++)
    (add hl (- #x10000 8))
    (add de (- #x10000 8))

    (preserve (hl de) (write "\rHL "))
    (call write-ireg-word/color++)
    (add hl 6)
    (add de 6)

    (preserve (hl de) (write " HL'"))
    (call write-ireg-word/color++)
    (add hl (- #x10000 8))
    (add de (- #x10000 8))

    (preserve (hl de) (write "\rBC "))
    (call write-ireg-word/color++)
    (add hl 6)
    (add de 6)

    (preserve (hl de) (write " BC'"))
    (call write-ireg-word/color++)
    (add hl (- #x10000 8))
    (add de (- #x10000 8))

    (preserve (hl de) (write "\rDE "))
    (call write-ireg-word/color++)
    (add hl 6)
    (add de 6)

    (preserve (hl de) (write " DE'"))
    (call write-ireg-word/color++)

    (preserve (hl de) (write "\rIX "))
    (call write-ireg-word/color++)

    (preserve (hl de) (write " IY "))
    (call write-ireg-word/color++)

    (jp write-newline))

  (define-fragment regs-diff-colors
    (input
      (hl expected-regs)
      (de actual-regs)
      (bc regs-mask))
    (output
      (hl2 expected-colors)
      (de2 actual-colors)
      (fz 1 no diff / 0 diff))
    (preserve (ix iy)
      (ld iyh 0)  ; for result
      (exx)
      (ld b 20)
      (loop-djnz
        (exx)

        ; Load expected reg
        (ld a (hl))
        (ld ixl a)
        (inc hl)

        ; Load actual reg
        (ld a (de))
        (ld ixh a)
        (inc de)

        ; Load mask
        (ld a (bc))
        (ld iyl a)
        (inc bc)

        ; Compare
        (ld a ixl)
        (and iyl)
        (ld ixl a)

        (ld a ixh)
        (and iyl)
        (xor ixl)

        (exx)

        ; Pick color
        (if z
          (then
            (ld a #x07)
            (ld (hl) a)
            (inc hl)
            (ld (de) a)
            (inc de))
          (else
            (ld iyh 1)
            (ld a #x02)
            (ld (hl) a)
            (inc hl)
            (ld a #x04)
            (ld (de) a)
            (inc de))))
      (ld a iyh))
    (or a)
    (ret))
)
