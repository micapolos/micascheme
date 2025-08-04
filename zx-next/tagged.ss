(library (zx-next tagged)
  (export
    tagged-byte tagged-word
    tag untag
    write-tagged-byte
    write-tagged-word
    write-tag)
  (import
    (zx-next core)
    (zx-next tag)
    (zx-next write))

  (define-expression (tagged-byte tag byte)
    (fxior (fxand byte #x1f) tag))

  (define-expression (tagged-word tag word)
    (fxior (fxand word #x1fff) (fxsll tag 8)))

  (define-ops (keywords a l de hl)
    ; a <- l tagged with a
    ((tag l)
      (or l))

    ; hl <- de tagged with a
    ((tag hl de)
      (or d)
      (ld h a)
      (ld l e))

    ; a <- tag
    ; l <- untagged a
    ; h <- input a
    ((untag l)
      (ld h a)
      (and tag-inv-mask)
      (ld l a)
      (ld a h)
      (and tag-mask))

    ; a <- tag
    ; de <- untagged hl
    ((untag de hl)
      (ld a h)
      (and tag-inv-mask)
      (ld d a)
      (ld e l)
      (ld a h)
      (and tag-mask))

    ((untag de bc)
      (ld a b)
      (and tag-inv-mask)
      (ld d a)
      (ld e c)
      (ld a b)
      (and tag-mask)))

  (define-fragment write-tagged-byte
    (input (a tagged-byte))
    (untag l)
    (preserve (hl) (call write-tag))
    (ld a l)
    (jp write-byte))

  (define-fragment write-tagged-word
    (input (hl tagged-word))
    (untag de hl)
    (ex de hl)
    (preserve (hl) (call write-tag))
    (jp write-word))
)
