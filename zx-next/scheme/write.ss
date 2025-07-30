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

  (define-fragment scheme-write
    (input (hlde value))
    (bit 0 l)
    (if z
      ; primitive
      (then
        (bit 1 l)
        (if z
          ; integer
          (then
            (bit 2 l)
            (if z
              ; byte in a
              (then
                (jp write-byte))
              ; word, low byte in a, high byte in h
              (else
                (ld l a)
                (jp write-word))))
          ; non-integer
          (else
            (bit 2 l)
            (if z
              ; null / char
              (then
                (bit 3 l)
                (if z
                  ; null
                  (then
                    (ld hl null-string)
                    (jp write-string))
                  ; char
                  (else
                    (preserve (af)
                      (ld hl char-prefix-string)
                      (call write-string))
                    (jp write-char))))
              ; boolean
              (else
                (bit 3 l)
                (if z
                  (then (ld hl false-string))
                  (else (ld hl true-string)))
                (jp write-string))))))
      ; pointer: bank in a, 4-byte aligned address in hl
      (else
        (mmu 7 a)
        (jp write-string)))
    (ret))
)
