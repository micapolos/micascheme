(library (zx-next regs)
  (export
    regs-size
    push-regs
    pop-regs
    write-regs)
  (import
    (zx-next core)
    (zx-next write)
    (zx-next interrupt))

  ; AFHLBCDE AFHLBCDE' IXIY PC SP IR
  (define-value regs-size 26)

  (define-ops
    ((pop-regs)
      (pop af)
      (pop hl)
      (pop bc)
      (pop de)
      (exx)
      (pop af)
      (pop hl)
      (pop bc)
      (pop de)
      (exx)
      (pop ix)
      (pop iy))

    ((push-regs)
      (push iy)
      (push ix)
      (exx)
      (push de)
      (push bc)
      (push hl)
      (push af)
      (exx)
      (push de)
      (push bc)
      (push hl)
      (push af)))

  ; TODO: Try to make it more compact.
  (define-asm write-regs
    (input (hl regs-pointer))

    (preserve (hl) (write "AF "))
    (call write-ihl++)
    (add hl 6)

    (preserve (hl) (write " AF'"))
    (call write-ihl++)
    (add hl (- #x10000 8))

    (preserve (hl) (write "\rHL "))
    (call write-ihl++)
    (add hl 6)

    (preserve (hl) (write " HL'"))
    (call write-ihl++)
    (add hl (- #x10000 8))

    (preserve (hl) (write "\rBC "))
    (call write-ihl++)
    (add hl 6)

    (preserve (hl) (write " BC'"))
    (call write-ihl++)
    (add hl (- #x10000 8))

    (preserve (hl) (write "\rDE "))
    (call write-ihl++)
    (add hl 6)

    (preserve (hl) (write " DE'"))
    (call write-ihl++)

    (preserve (hl) (write "\rIX "))
    (call write-ihl++)

    (preserve (hl) (write " IY "))
    (call write-ihl)

    (preserve (hl) (write "\rPC "))
    (call write-ihl++)

    (preserve (hl) (write " SP "))
    (call write-ihl)

    (preserve (hl) (write "\rIR "))
    (call write-ihl)

    (jp write-newline))
)
