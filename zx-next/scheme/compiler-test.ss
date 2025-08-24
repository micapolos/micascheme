(import
  (micascheme)
  (syntax lookup)
  (zx-next scheme compiler)
  (prefix (zx-next scheme keywords) %)
  (prefix (zx-next scheme prims) %%))

(check-compile-op (empty-lookup)
  (%asm (%%inc %%a) (%%ret))
  (begin (%%begin (%%inc %%a) (%%ret))))

(check-compile-op (empty-lookup)
  (%begin)
  (begin (%%begin)))

(check-compile-op (empty-lookup)
  (%begin 1 "foo" (%quote bar))
  (begin
    (%%define-fragment $string_1 (%%dz "foo"))
    (%%define-fragment $symbol_0 (%%dz "bar"))
    (%%begin
      (%%load-value (%%byte-value 1))
      (%%load-value (%%string-value $string_1))
      (%%load-value (%%symbol-value $symbol_0)))))

(check-compile-op (empty-lookup)
  #x12
  (begin (%%load-value (%%byte-value #x12))))

(check-compile-op (empty-lookup)
  #x1234
  (begin (%%load-value (%%word-value #x1234))))

(check-compile-op (empty-lookup)
  #f
  (begin (%%load-value (%%false-value))))

(check-compile-op (empty-lookup)
  #f
  (begin (%%load-value (%%false-value))))

(check-compile-op (empty-lookup)
  (%quote ())
  (begin (%%load-value (%%null-value))))

(check-compile-op (empty-lookup)
  #\a
  (begin (%%load-value (%%char-value #\a))))

(check-compile-op (empty-lookup)
  "foo"
  (begin
    (%%define-fragment $string_0 (%%dz "foo"))
    (%%load-value (%%string-value $string_0))))

(check-compile-op (empty-lookup)
  (%quote foo)
  (begin
    (%%define-fragment $symbol_0 (%%dz "foo"))
    (%%load-value (%%symbol-value $symbol_0))))

(check-compile-op (empty-lookup)
  (%cons #x12 #f)
  (begin
    (%%begin
      (%%load-value (%%false-value))
      (%%push-top)
      (%%load-value (%%byte-value 18))
      (%%cons))))

(check-compile-op (empty-lookup)
  (%write #x12)
  (begin
    (%%begin
      (%%load-value (%%byte-value #x12))
      (%%write))))

(check-compile-op (empty-lookup)
  (%if #x12 #x13 #x14)
  (begin
    (%%begin
      (%%load-value (%%byte-value #x12))
      (%%if-true
        (%%load-value (%%byte-value #x13))
        (%%load-value (%%byte-value #x14))))))

(check-compile-op (empty-lookup)
  (%when #x12 #x13)
  (begin
    (%%begin
      (%%load-value (%%byte-value #x12))
      (%%when-true (%%begin (%%load-value (%%byte-value #x13)))))))

(check-compile-op (empty-lookup)
  (%quote (circle (label "my circle") (radius 10) (center (point (x 10) (y 20)))))
  (begin
    (%%define-fragment $symbol_7 (%%dz "circle"))
    (%%define-fragment $symbol_6 (%%dz "label"))
    (%%define-fragment $string_5 (%%dz "my circle"))
    (%%define-fragment $symbol_4 (%%dz "radius"))
    (%%define-fragment $symbol_3 (%%dz "center"))
    (%%define-fragment $symbol_2 (%%dz "point"))
    (%%define-fragment $symbol_1 (%%dz "x"))
    (%%define-fragment $symbol_0 (%%dz "y"))
    (%%begin
      (%%begin
        (%%begin
          (%%begin
            (%%load-value (%%null-value))
            (%%push-top)
            (%%begin
              (%%begin
                (%%load-value (%%null-value))
                (%%push-top)
                (%%begin
                  (%%begin
                    (%%begin
                      (%%load-value (%%null-value))
                      (%%push-top)
                      (%%begin
                        (%%begin
                          (%%load-value (%%null-value))
                          (%%push-top)
                          (%%load-value (%%byte-value 20))
                          (%%cons))
                        (%%push-top)
                        (%%load-value (%%symbol-value $symbol_0))
                        (%%cons))
                      (%%cons))
                    (%%push-top)
                    (%%begin
                      (%%begin
                        (%%load-value (%%null-value))
                        (%%push-top)
                        (%%load-value (%%byte-value 10))
                        (%%cons))
                      (%%push-top)
                      (%%load-value (%%symbol-value $symbol_1))
                      (%%cons))
                    (%%cons))
                  (%%push-top)
                  (%%load-value (%%symbol-value $symbol_2))
                  (%%cons))
                (%%cons))
              (%%push-top)
              (%%load-value (%%symbol-value $symbol_3))
              (%%cons))
            (%%cons))
          (%%push-top)
          (%%begin
            (%%begin
              (%%load-value (%%null-value))
              (%%push-top)
              (%%load-value (%%byte-value 10))
              (%%cons))
            (%%push-top)
            (%%load-value (%%symbol-value $symbol_4))
            (%%cons))
          (%%cons))
        (%%push-top)
        (%%begin
          (%%begin
            (%%load-value (%%null-value))
            (%%push-top)
            (%%load-value (%%string-value $string_5))
            (%%cons))
          (%%push-top)
          (%%load-value (%%symbol-value $symbol_6))
          (%%cons))
        (%%cons))
      (%%push-top)
      (%%load-value (%%symbol-value $symbol_7))
      (%%cons))))


