(import
  (micascheme)
  (zx-next scheme compiler)
  (syntax lookup))

(check-expr->asm (empty-lookup)
  (top 2)
  (%dup-value 2))

(check-expr->asm (empty-lookup)
  (byte #x12)
  (%push-byte #x12))

(check-expr->asm (empty-lookup)
  (word #x1234)
  (%push-word #x1234))

(check-expr->asm (empty-lookup)
  (byte+
    (byte #x12)
    (byte-
      (byte #x34)
      (byte #x56)))
  (%begin
    (%begin
      (%push-byte 86)
      (%push-byte 52)
      (%byte-sub))
    (%push-byte 18)
    (%byte-add)))

(check-expr->asm (empty-lookup)
  (lets
    (byte #x12)
    (byte #x34)
    (byte+ (top 0) (top 1)))
  (%begin
    (%push-byte #x12)
    (%push-byte #x34)
    (%begin
      (%dup-value 1)
      (%dup-value 0)
      (%byte-add))
    (%pop-value)
    (%pop-value)))
