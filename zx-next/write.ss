(library (zx-next write)
  (export
    write-init
    write-char
    write-b-chars
    write-bc-chars
    write-string
    writeln-string
    write-nibble
    write-byte
    write-word
    write-newline
    write-regs
    write-mem
    write-mem-line
    write
    write-ink
    write-paper
    writeln
    writeln-error
    writeln-ok)
  (import
    (zx-next core)
    (only (micascheme) syntax-rules char? string? datum)
    (u))

  (define-fragment write-init
    (input (hl write-char-address))
    (ex de hl)
    (ld hl (+ write-char 1))
    (ld (hl) e)
    (inc hl)
    (ld (hl) d)
    (ret))

  (define-fragment write-char
    (input (a char))
    (jp #x0008))

  (define-fragment write-b-chars
    (input (hl address) (b count))
    (loop-djnz
      (ld a (hl))
      (inc hl)
      (preserve (hl bc) (call write-char)))
    (ret))

  (define-fragment write-bc-chars
    (input (hl address) (bc count))
    (loop
      (ld a (hl))
      (inc hl)
      (preserve (hl bc) (call write-char))
      (dec bc)
      (ld a b)
      (or c)
      (while nz))
    (ret))

  (define-fragment write-string
    (input (hl string))
    (loop
      (ld a (hl))
      (inc hl)
      (or a)
      (ret z)
      (preserve (hl) (call write-char))))

  (define-fragment writeln-string
    (input (hl string))
    (call write-string)
    (jp write-newline))

  (define-fragment write-nibble
    (input (a nibble))
    (cp 10)
    (jp c digit)
    (add (- #\A 10))
    (jp write)
    digit
    (add #\0)
    write
    (jp write-char))

  (define-fragment write-byte
    (input (a byte))
    (preserve (af)
      (swapnib)
      (and #x0f)
      (call write-nibble))
    (and #x0f)
    (jp write-nibble))

  (define-fragment write-word
    (input (hl word))
    (preserve (hl)
      (ld a h)
      (call write-byte))
    (ld a l)
    (jp write-byte))

  (define-fragment write-newline
    (ld a #x0d)
    (jp write-char))

  (define-ops
    ((write-ink n)
      (ld a #x10)
      (call write-char)
      (ld a n)
      (call write-char))
    ((write-paper n)
      (ld a #x11)
      (call write-char)
      (ld a n)
      (call write-char)))

  (define-op-syntax write
    (syntax-rules (a b c d e h l af bc de hl sp)
      ((_ a) (begin (call write-byte)))
      ((_ b) (begin (ld a b) (call write-byte)))
      ((_ c) (begin (ld a c) (call write-byte)))
      ((_ d) (begin (ld a d) (call write-byte)))
      ((_ e) (begin (ld a e) (call write-byte)))
      ((_ h) (begin (ld a h) (call write-byte)))
      ((_ l) (begin (ld a l) (call write-byte)))
      ((_ af) (begin (push af) (pop hl) (call write-word)))
      ((_ bc) (begin (ld h b) (ld l c) (call write-word)))
      ((_ de) (begin (ld h d) (ld l e) (call write-word)))
      ((_ hl) (begin (call write-word)))
      ((_ sp) (begin (ld hl 0) (add hl sp) (call write-word)))
      ((_ ch)
        (char? (datum ch))
        (begin
          (ld a ch)
          (call write-char)))
      ((_ u8)
        (u8? (datum u8))
        (begin
          (ld a u8)
          (call write-byte)))
      ((_ u16)
        (u16? (datum u16))
        (begin
          (ld hl u16)
          (call write-word)))
      ((_ s)
        (string? (datum s))
        (with-labels (here)
          (call here)
          (dz s)
          here
          (pop hl)
          (call write-string)))
      ((_ s ...)
        (begin (preserve (af bc de hl) (write s)) ...))))

  (define-op (writeln s ...)
    (write s ...)
    (call write-newline))

  (define-op (writeln-error s ...)
    (preserve (af bc de hl)
      (write-ink 4)
      (write "[ERROR] ")
      (write-ink 7))
    (writeln s ...))

  (define-op (writeln-ok s ...)
    (preserve (af bc de hl)
      (write-ink 2)
      (write "[OK] ")
      (write-ink 7))
    (writeln s ...))

  ; TODO === Move to (zx-next debug) ===

  (define-fragment write-regs
    (writeln "AF " af "\rBC " bc "\rDE " de "\rHL " hl "\rSP " sp)
    (ret))

  (define-fragments
    (write-mem-columns (db 16)))

  (define-fragment write-mem
    (input (hl address) (bc length))
    ; bc = advanced length
    ; a = line length
    (loop
      (preserve (hl)
        (ld h b)
        (ld l c)
        (ld a (write-mem-columns))
        (ld d 0)
        (ld e a)
        (ld a l)
        (rcf)
        (sbc hl de)
        (if c
          (then (ld hl 0))
          (else (ld a e)))
        (ld b h)
        (ld c l))

      (preserve (bc)
        (ld c a)
        (call write-mem-line))

      ; TODO: implement using _do / _while
      (ld a b)
      (or c)
      (ret z)))

  (define-fragment write-mem-line
    (input (hl address) (c length))
    (output (hl advanced-address))

    (preserve (bc hl)
      (call write-word)
      (write "   "))

    (preserve (hl)
      (preserve (bc)
        (ld a (write-mem-columns))
        (ld b a)
        (loop-djnz
          (ld d (hl))
          (inc hl)
          (preserve (hl)
            (xor a)
            (or c)
            (if z
              (then
                (preserve (bc)
                  (write "   ")))
              (else
                (preserve (bc)
                  (ld a d)
                  (call write-byte)
                  (ld a #\space)
                  (call write-char))
                (dec c))))))
        (write "  "))

    (ld a (write-mem-columns))
    (ld b a)
    (loop-djnz
      (ld d (hl))
      (inc hl)
      (preserve (hl)
        (xor a)
        (or c)
        (if z
          (then
            (preserve (bc)
              (ld a #\space)
              (call write-char)))
          (else
            (ld a d)
            (cp #x20)
            (preserve (bc)
              (if c
                (then
                  (ld a #\.)
                  (call write-char))
                (else
                  (cp #x7f)
                  (if c
                    (then
                      (call write-char))
                    (else
                      (ld a #\.)
                      (call write-char))))))
            (dec c)))))

    (preserve (hl)
      (call write-newline))
    (ret))
)
