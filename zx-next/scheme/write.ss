(library (zx-next scheme write)
  (export scheme-write)
  (import
    (zx-next core)
    (zx-next mmu)
    (zx-next write)
    (zx-next scheme value))

  (define-fragments
    (byte-string      (dz "#<byte>"))
    (word-string      (dz "#<word>"))
    (pointer-string   (dz "#<pointer>")))

  (define-fragment scheme-write
    (input (hlde value))
    (bit 0 l)
    (if z
      ; primitive
      (then
        (bit 1 l)
        (if z
          ; byte in d
          (then
            (ld hl byte-string)
            (jp write-string))
          ; word, low byte in d, high byte in h
          (else
            (ld hl byte-string)
            (jp write-string))))
      ; pointer: bank in d, address in hl
      (else
        (ld a d)
        (mmu 7 a)
        (jp write-string)))
    (ret))
)
