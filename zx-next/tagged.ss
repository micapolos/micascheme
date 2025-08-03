(library (zx-next tagged)
  (export
    tagged->tag/byte
    tagged->tag/word
    write-tagged-byte
    write-tagged-word
    write-tag)
  (import
    (zx-next core)
    (zx-next tag)
    (zx-next write))

  (define-fragment tagged->tag/byte
    (input (a tagged))
    (output (a tag) (l byte))
    (ld h a)
    (and tag-inv-mask)
    (ld l a)
    (ld a h)
    (and tag-mask)
    (ret))

  (define-fragment tagged->tag/word
    (input (hl tagged-word))
    (output (a tag) (de word))
    (ld a h)
    (and tag-inv-mask)
    (ld d a)
    (ld e l)
    (ld a h)
    (and tag-mask)
    (ret))

  (define-fragment write-tagged-byte
    (input (a tagged-byte))
    (call tagged->tag/byte)
    (preserve (hl) (call write-tag))
    (ld a l)
    (jp write-byte))

  (define-fragment write-tagged-word
    (input (hl tagged-word))
    (call tagged->tag/word)
    (ex de hl)
    (preserve (hl) (call write-tag))
    (jp write-word))
)
