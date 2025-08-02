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
  (%%local 2)
  (%local-value 2))

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
    (%%byte+ (%%local 0) (%%local 4)))
  (%begin
    (%push-byte #x12)
    (%push-byte #x34)
    (%begin
      (%local-value 4)
      (%local-value 0)
      (%byte-add))
    (%pop-value)
    (%pop-value)))

(check-stmt->asm (empty-lookup)
  (%%write (%%byte #x12))
  (%begin
    (%push-byte #x12)
    (%call %println)))

(check-stmt->asm (empty-lookup)
  (%%write-stack)
  (%write-stack))

(check-stmt->asm (empty-lookup)
  (%%begin
    (%%write (%%byte #x12))
    (%%write (%%byte #x34)))
  (%begin
    (%begin
      (%push-byte 18)
      (%call %println))
    (%begin
      (%push-byte 52)
      (%call %println))))

(check-stmt->asm (empty-lookup)
  (%%lets
    (%%byte #x12)
    (%%byte #x34)
    (%%begin
      (%%write (%%local 0))
      (%%write (%%local 4))))
  (%begin
    (%push-byte #x12)
    (%push-byte #x34)
    (%begin
      (%begin
        (%local-value 0)
        (%call %println))
      (%begin
        (%local-value 4)
        (%call %println)))
    (%pop-value)
    (%pop-value)))

