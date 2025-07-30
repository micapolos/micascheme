(library (zx-next scheme write)
  (export scheme-write)
  (import
    (zx-next core)
    (zx-next mmu)
    (zx-next write)
    (zx-next scheme value))

  (define-fragments
    (null-string         (dz "()"))
    (false-string        (dz "#f"))
    (true-string         (dz "#t"))
    (char-prefix-string  (dz "#\\")))

  (define-fragment scheme-write-byte
    (jp write-byte))

  (define-fragment scheme-write-word
    (ex de hl)
    (ld l a)
    (jp write-word))

  (define-fragment scheme-write-char
    (ex de hl)
    (preserve (af)
      (ld hl char-prefix-string)
      (call write-string))
    (jp write-char))

  (define-fragment scheme-write-false
    (ld hl false-string)
    (jp write-string))

  (define-fragment scheme-write-true
    (ld hl true-string)
    (jp write-string))

  (define-fragment scheme-write-null
    (ld hl null-string)
    (jp write-string))

  (define-fragment scheme-write-pointer
    (ex de hl)
    (mmu 7 a)
    (ld a h)
    (or #b11100000)
    (ld h a)
    (dec hl)
    (ld a (hl))
    (inc hl)
    (bit 0 a)
    (if z
      (then (jp scheme-write-symbol))
      (else (jp scheme-write-string))))

  (define-fragment scheme-write-symbol
    (jp write-string))

  (define-fragment scheme-write-string
    (preserve (hl)
      (ld a #\")
      (call write-char))
    (call write-string)
    (ld a #\")
    (jp write-char))

  (define-fragment write-dispatch-table
    (dw scheme-write-byte)      ; 0000
    (dw scheme-write-pointer)   ; 0001
    (dw scheme-write-null)      ; 0010
    (dw scheme-write-pointer)   ; 0011
    (dw scheme-write-word)      ; 0100
    (dw scheme-write-pointer)   ; 0101
    (dw scheme-write-false)     ; 0110
    (dw scheme-write-pointer)   ; 0111
    (dw scheme-write-byte)      ; 1000
    (dw scheme-write-pointer)   ; 1001
    (dw scheme-write-char)      ; 1010
    (dw scheme-write-pointer)   ; 1011
    (dw scheme-write-word)      ; 1100
    (dw scheme-write-pointer)   ; 1101
    (dw scheme-write-true)      ; 1110
    (dw scheme-write-pointer))  ; 1111

  (define-fragment scheme-write
    (input (hla value))
    ; save HL in DE, and A in B
    (ex de hl)
    (ld b a)

    ; load tag offset
    (ld a e)
    (and #xf)
    (add a)

    ; load dispatch offset
    (ld hl write-dispatch-table)
    (add hl a)

    ; load write address
    (ld a (hl))
    (inc hl)
    (ld h (hl))
    (ld l a)

    ; restore A and dispatch (HL is in DE)
    (ld a b)
    (jp (hl)))
)
