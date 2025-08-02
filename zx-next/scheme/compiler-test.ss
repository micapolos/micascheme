(import
  (micascheme)
  (prefix (zx-next scheme compiler-keywords) %%)
  (zx-next scheme compiler)
  (syntax lookup))

(check-expr->asm (empty-lookup)
  #t
  (%push-true))

(check-expr->asm (empty-lookup)
  #f
  (%push-false))

(check-expr->asm (empty-lookup)
  ()
  (%push-null))

(check-expr->asm (empty-lookup)
  (%%top 2)
  (%dup-value 2))

(check-expr->asm (empty-lookup)
  (%%byte #x12)
  (%push-byte #x12))

(check-expr->asm (empty-lookup)
  (%%word #x1234)
  (%push-word #x1234))

(check-expr->asm (empty-lookup)
  (%%byte+
    (%%byte #x12)
    (%%byte-
      (%%byte #x34)
      (%%byte #x56)))
  (%begin
    (%begin
      (%push-byte 86)
      (%push-byte 52)
      (%byte-sub))
    (%push-byte 18)
    (%byte-add)))

(check-expr->asm (empty-lookup)
  (%%lets
    (%%byte #x12)
    (%%byte #x34)
    (%%byte+ (%%top 0) (%%top 1)))
  (%begin
    (%push-byte #x12)
    (%push-byte #x34)
    (%begin
      (%dup-value 1)
      (%dup-value 0)
      (%byte-add))
    (%pop-value)
    (%pop-value)))

(check-stmt->asm (empty-lookup)
  (%%write (%%byte #x12))
  (%begin (%push-byte #x12) (%write-value)))

(check-stmt->asm (empty-lookup)
  (%%write-stack)
  (%println-stack))

(check-stmt->asm (empty-lookup)
  (%%begin
    (%%write (%%byte #x12))
    (%%write (%%byte #x34)))
  (%begin
    (%begin
      (%push-byte 18)
      (%write-value))
    (%begin
      (%push-byte 52)
      (%write-value))))

(check-stmt->asm (empty-lookup)
  (%%lets
    (%%byte #x12)
    (%%byte #x34)
    (%%begin
      (%%write (%%top 0))
      (%%write (%%top 1))))
  (%begin
    (%push-byte #x12)
    (%push-byte #x34)
    (%begin
      (%begin
        (%dup-value 0)
        (%write-value))
      (%begin
        (%dup-value 1)
        (%write-value)))
    (%pop-value)
    (%pop-value)))

