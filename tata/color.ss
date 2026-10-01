(library (tata color)
  (export
    color-rgba
    rgba-color)
  (import
    (scheme)
    (syntax))

  (define-rule-syntax (color-rgba u32)
    (let
      (($u32 u32))
      (values
        (fxand (fxsrl $u32 16) #xff)
        (fxand (fxsrl $u32 8) #xff)
        (fxand u32 #xff)
        (fxand (fxsrl $u32 24) #xff))))

  (define-rule-syntax (rgba-color r g b a)
    (fxlogior
      (fxsll r 16)
      (fxsll g 8)
      b
      (fxsll b 24)))
)
