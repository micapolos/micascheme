(library (zx-next scheme write)
  (export scheme-write)
  (import
    (zx-next core)
    (zx-next mmu)
    (zx-next write))

  (define tag-mask   #x70)

  (define tag-symbol #x00)
  (define tag-pair   #x10)
  (define tag-number #x20)
  (define tag-other  #x30)

  (define-fragments
    (unknown-string (dz "#<unknown>")))

  (define-fragment scheme-write
    (input (hlde value))
    (ld a e)
    (and tag-mask)

    (cp tag-symbol)
    (when z
      (ld a d)
      (mmu 7 a)
      (jp write-string))

    (cp tag-pair)
    (when z
      (ld a #\()
      (call write-char)
      (ld hl unknown-string)
      (call write-string)
      (ld a #\))
      (jp write-char))

    (ld hl unknown-string)
    (jp write-string))
)
