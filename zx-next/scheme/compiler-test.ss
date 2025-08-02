(import
  (except (micascheme) write)
  (zx-next scheme compiler)
  (syntax lookup))

(check-scoped-expr->asm (empty-lookup)
  (scoped () () (byte #x12))
  (%push-byte #x12))

(check-scoped-expr->asm (empty-lookup)
  (scoped () () (word #x1234))
  (%push-word #x1234))

(check-scoped-expr->asm (empty-lookup)
  (scoped () ()
    (byte+
      (byte #x12)
      (byte-
        (byte #x34)
        (byte #x56))))
  (%begin
    (%begin
      (%push-byte 86)
      (%push-byte 52)
      (%byte-sub))
    (%push-byte 18)
    (%byte-add)))
