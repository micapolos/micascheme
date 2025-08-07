(import
  (micascheme)
  (syntax lookup)
  (zx-next scheme compiler)
  (prefix (zx-next scheme keywords) %)
  (prefix (zx-next scheme prims) %%))

(check-compile-statement (empty-lookup)
  (2 #x12)
  (begin (%%load-value (%%byte-value 2 #x12))))

(check-compile-statement (empty-lookup)
  (2 #x1234)
  (begin (%%load-value (%%word-value 2 #x1234))))

(check-compile-statement (empty-lookup)
  (2 #f)
  (begin (%%load-value (%%false-value 2))))

(check-compile-statement (empty-lookup)
  (2 #f)
  (begin (%%load-value (%%false-value 2))))

(check-compile-statement (empty-lookup)
  (2 ())
  (begin (%%load-value (%%null-value 2))))

(check-compile-statement (empty-lookup)
  (2 #\a)
  (begin (%%load-value (%%char-value 2 #\a))))

(check-compile-statement (empty-lookup)
  (2 "foo")
  (begin
    (%%define-fragment $string_0 (%%dz "foo"))
    (%%load-value (%%string-value 2 $string_0))))

(check-compile-statement (empty-lookup)
  (2 (%quote foo))
  (begin
    (%%define-fragment $symbol_0 (%%dz "foo"))
    (%%load-value (%%symbol-value 2 $symbol_0))))

(check-compile-statement (empty-lookup)
  (2 (%cons #x12 #f))
  (begin
    (%%begin
      (%%load-value (%%byte-value 2 #x12))
      (%%push-value)
      (%%load-value (%%false-value 0))
      (%%cons))))
