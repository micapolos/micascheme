(library (zx-next tag)
  (export
    tag-mask
    tag-inv-mask
    write-tag)
  (import
    (zx-next core)
    (zx-next write))

  ; Tag is a 3-bit piece of information encoded in bits 7 ... 5.

  (define-values
    (tag-mask     #b11100000)
    (tag-inv-mask #b00011111))

  (define-fragment write-tag
    (input (a tag))
    (dup 3 (rlca))
    (call write-nibble)
    (ld a #\:)
    (jp write-char))
)
