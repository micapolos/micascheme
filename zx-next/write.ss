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
    write-byte-literal
    write-word
    write-word-literal
    write-newline
    write-mem
    write-mem-line
    write
    write/preserve
    write-ink
    write-paper
    writeln
    writeln-error
    writeln-ok

    write-a
    write-f
    write-h
    write-l
    write-b
    write-c
    write-d
    write-e
    write-af
    write-hl
    write-bc
    write-de
    write-sp
    write-ihl
    write-ihl++

    write-a/preserve
    write-f/preserve
    write-h/preserve
    write-l/preserve
    write-b/preserve
    write-c/preserve
    write-d/preserve
    write-e/preserve
    write-af/preserve
    write-hl/preserve
    write-bc/preserve
    write-de/preserve
    write-sp/preserve
    write-ihl/preserve
    write-ihl++/preserve)
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

  (define-fragment write-char/preserve
    (input (a char))
    (preserve-main/alternate (call write-char)))

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

  (define-fragment write-string/preserve
    (input (hl string))
    (preserve-main/alternate (call write-string)))

  (define-fragment writeln-string
    (input (hl string))
    (call write-string)
    (jp write-newline))

  (define-fragment write-nibble
    (input (a nibble))
    (cp 10)
    (jp c digit)
    (add (- #\a 10))
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

  (define-fragment write-byte-literal
    (input (a byte))
    (preserve (af)
      (ld a #\$)
      (call write-char))
    (jp write-byte))

  (define-fragment write-byte-literal/preserve
    (input (a byte))
    (preserve-main/alternate (call write-byte-literal)))

  (define-fragment write-word
    (input (hl word))
    (preserve (hl)
      (ld a h)
      (call write-byte))
    (ld a l)
    (jp write-byte))

  (define-fragment write-word-literal
     (input (hl word))
     (preserve (hl)
       (ld a #\$)
       (call write-char))
     (jp write-word))

  (define-fragment write-word-literal/preserve
    (input (hl word))
    (preserve-main/alternate (call write-word-literal)))

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

  (define-fragments
    (write-a (jp write-byte-literal))
    (write-f (push af) (pop hl) (jp write-byte-literal))
    (write-h (ld a h) (jp write-byte-literal))
    (write-l (ld a l) (jp write-byte-literal))
    (write-b (ld a b) (jp write-byte-literal))
    (write-c (ld a c) (jp write-byte-literal))
    (write-d (ld a d) (jp write-byte-literal))
    (write-e (ld a e) (jp write-byte-literal))

    (write-af (push af) (pop hl) (jp write-word-literal))
    (write-hl (jp write-word-literal))
    (write-bc (ld h b) (ld l c) (jp write-word-literal))
    (write-de (ld h d) (ld l e) (jp write-word-literal))

    (write-ix (push ix) (pop hl) (jp write-word-literal))
    (write-iy (push iy) (pop hl) (jp write-word-literal))

    (write-sp (ld hl 0) (add hl sp) (jp write-word-literal))

    (write-ihl
      (ld e (hl))
      (inc hl)
      (ld d (hl))
      (ex de hl)
      (jp write-word-literal))

    (write-ihl++
      (ld e (hl))
      (inc hl)
      (ld d (hl))
      (inc hl)
      (ex de hl)
      (preserve (de) (call write-word-literal))
      (ex de hl)
      (ret)))

  (define-fragments
    (write-a/preserve (preserve-main/alternate (call write-a)))
    (write-f/preserve (preserve-main/alternate (call write-f)))
    (write-h/preserve (preserve-main/alternate (call write-h)))
    (write-l/preserve (preserve-main/alternate (call write-l)))
    (write-b/preserve (preserve-main/alternate (call write-b)))
    (write-c/preserve (preserve-main/alternate (call write-c)))
    (write-d/preserve (preserve-main/alternate (call write-d)))
    (write-e/preserve (preserve-main/alternate (call write-e)))

    (write-af/preserve (preserve-main/alternate (call write-af)))
    (write-hl/preserve (preserve-main/alternate (call write-hl)))
    (write-bc/preserve (preserve-main/alternate (call write-bc)))
    (write-de/preserve (preserve-main/alternate (call write-de)))

    (write-ix/preserve (preserve-main/alternate (call write-ix)))
    (write-iy/preserve (preserve-main/alternate (call write-iy)))

    (write-sp/preserve (preserve-main/alternate (call write-sp)))

    (write-ihl/preserve (preserve-main/alternate (call write-ihl)))
    (write-ihl++/preserve (preserve-main/alternate (call write-ihl++))))

  (define-op-syntax write
    (syntax-rules (a f h l b c d e af hl bc de sp)
      ((_ a) (call write-a))
      ((_ f) (call write-f))
      ((_ h) (call write-h))
      ((_ l) (call write-l))
      ((_ b) (call write-b))
      ((_ c) (call write-c))
      ((_ d) (call write-d))
      ((_ e) (call write-e))
      ((_ af) (call write-af))
      ((_ hl) (call write-hl))
      ((_ bc) (call write-bc))
      ((_ de) (call write-de))
      ((_ sp) (call write-sp))
      ((_ ch)
        (char? (datum ch))
        (begin (ld a ch) (call write-char)))
      ((_ u8)
        (u8? (datum u8))
        (begin (ld a u8) (call write-byte-literal)))
      ((_ u16)
        (u16? (datum u16))
        (begin (ld hl u16) (call write-word-literal)))
      ((_ s)
        (string? (datum s))
        (with-labels (here)
          (call here)
          (dz s)
          here
          (pop hl)
          (call write-string)))
      ((_ s ...)
        (begin (write/preserve s) ...))))

  (define-op-syntax write/preserve
    (syntax-rules (a f h l b c d e af hl bc de sp)
      ((_ a) (call write-a/preserve))
      ((_ f) (call write-f/preserve))
      ((_ h) (call write-h/preserve))
      ((_ l) (call write-l/preserve))
      ((_ b) (call write-b/preserve))
      ((_ c) (call write-c/preserve))
      ((_ d) (call write-d/preserve))
      ((_ e) (call write-e/preserve))
      ((_ af) (call write-af/preserve))
      ((_ hl) (call write-hl/preserve))
      ((_ bc) (call write-bc/preserve))
      ((_ de) (call write-de/preserve))
      ((_ sp) (call write-sp/preserve))
      ((_ ch)
        (char? (datum ch))
        (preserve (af) (write ch)))
      ((_ u8)
        (u8? (datum u8))
        (preserve (af) (write u8)))
      ((_ u16)
        (u16? (datum u16))
        (preserve (hl) (write u16)))
      ((_ s)
        (string? (datum s))
        (preserve (hl) (write s)))
      ((_ s ...)
        (begin (write/preserve s) ...))))

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
