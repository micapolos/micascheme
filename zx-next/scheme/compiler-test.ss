(import
  (micascheme)
  (syntax lookup)
  (zx-next scheme compiler)
  (prefix (zx-next scheme keywords) %)
  (prefix (zx-next scheme prims) %%))

(check-compile-op (empty-lookup)
  (2 (%begin))
  (begin (%%begin)))

(check-compile-op (empty-lookup)
  (2 (%begin 1 "foo" (%quote bar)))
  (begin
   (%%define-fragment $string_0 (%%dz "foo"))
   (%%define-fragment $symbol_1 (%%dz "bar"))
   (%%begin
    (%%load-value (%%byte-value 2 1))
    (%%begin
      (%%load-value (%%string-value 0 $string_0))
      (%%begin
        (%%load-value (%%symbol-value 0 $symbol_1))
        (%%begin))))))

(check-compile-op (empty-lookup)
  (2 #x12)
  (begin (%%load-value (%%byte-value 2 #x12))))

(check-compile-op (empty-lookup)
  (2 #x1234)
  (begin (%%load-value (%%word-value 2 #x1234))))

(check-compile-op (empty-lookup)
  (2 #f)
  (begin (%%load-value (%%false-value 2))))

(check-compile-op (empty-lookup)
  (2 #f)
  (begin (%%load-value (%%false-value 2))))

(check-compile-op (empty-lookup)
  (2 ())
  (begin (%%load-value (%%null-value 2))))

(check-compile-op (empty-lookup)
  (2 #\a)
  (begin (%%load-value (%%char-value 2 #\a))))

(check-compile-op (empty-lookup)
  (2 "foo")
  (begin
    (%%define-fragment $string_0 (%%dz "foo"))
    (%%load-value (%%string-value 2 $string_0))))

(check-compile-op (empty-lookup)
  (2 (%quote foo))
  (begin
    (%%define-fragment $symbol_0 (%%dz "foo"))
    (%%load-value (%%symbol-value 2 $symbol_0))))

(check-compile-op (empty-lookup)
  (2 (%cons #x12 #f))
  (begin
    (%%begin
      (%%load-value (%%byte-value 2 #x12))
      (%%push-top)
      (%%load-value (%%false-value 0))
      (%%cons))))

(check-compile-op (empty-lookup)
  (2 (%write #x12))
  (begin
    (%%begin
      (%%load-value (%%byte-value 2 #x12))
      (%%write))))

; ==================================================

(check-compile-define (empty-lookup)
  (%define foo (%write (%cons "foo" (%quote bar))))
  (begin
    (%%define-fragment $symbol_0 (%%dz "bar"))
    (%%define-fragment $string_1 (%%dz "foo"))
    (%%define-proc (foo)
      (%%begin
        (%%begin
          (%%load-value (%%string-value 2 $string_1))
          (%%push-top)
          (%%load-value (%%symbol-value 0 $symbol_0))
          (%%cons))
        (%%write)))))




