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
    push-regs
    pop-regs
    write-regs

    preserve-regs
    capture-regs
    captured-regs)
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

  (define-ops
    ((pop-regs)
      (pop af)
      (pop hl)
      (pop bc)
      (pop de)
      (exx) (ex af)
      (pop af)
      (pop hl)
      (pop bc)
      (pop de)
      (exx) (ex af)
      (pop ix)
      (pop iy))

    ((push-regs)
      (push iy)
      (push ix)
      (exx) (ex af)
      (push de)
      (push bc)
      (push hl)
      (push af)
      (exx) (ex af)
      (push de)
      (push bc)
      (push hl)
      (push af)))

  (define-op (preserve-regs body ...)
    (push-regs)
    body ...
    (pop-regs))

  (define-op (capture-regs)
    (push-regs)
    (ld de captured-regs)
    (ld hl 0)
    (add hl sp)
    (ld bc regs-size)
    (ldir)
    (pop-regs))

  ; TODO: Try to make it more compact.
  (define-asm write-regs
    (input (hl regs-pointer))
    (ld de reg-names)

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

    (jp write-newline))
)
