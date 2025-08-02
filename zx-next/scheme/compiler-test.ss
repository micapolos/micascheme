(import
  (except (micascheme) write)
  (zx-next scheme compiler)
  (syntax lookup))

(check-scoped-expr->asm (empty-lookup)
  (scoped
    (arg-1 arg-2 arg-3 arg-4 arg-3)
    (local-1 local-2 local-3 local-4 local-3)
    local-3)
  (%dup-value 2))

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

(check-scoped-expr->asm (empty-lookup)
  (scoped () ()
    (let
      (
        ($byte-1 (byte #x12))
        ($byte-2 (byte #x34)))
      (byte+ $byte-1 $byte-2)))
  (%begin
    (%push-byte #x12)
    (%push-byte #x34)
    (%begin
      (%dup-value 1)
      (%dup-value 0)
      (%byte-add))
    (%pop-value)
    (%pop-value)))


