(import
  (except (micascheme) write)
  (zx-next scheme compiler)
  (syntax lookup))

(check-syntax->expr (empty-lookup)
  (byte+ (byte #x12) (byte #x34))
  (%push-byte 52) (%push-byte 18) (%byte-add))
