(library (emu reg)
  (export
    define-idx
    idx-size
    define-reg-8
    define-reg-16
    define-mux-8)
  (import
    (scheme)
    (lets)
    (syntax)
    (syntaxes)
    (emu math)
    (emu mem)
    (emu internal))

  (define-internal idx-size)

  (define-rules-syntaxes
    ((define-idx $id $size)
      (begin
        (define $box (box 0))
        (define-rules-syntax
          (($id) (unbox $box))
          (($id $expr) (set-box! $box $expr)))
        (define-idx-size $id $size)))

    ((define-reg-8 $id)
      (define-idx $id #x100))

    ((define-reg-16 $id)
      (define-idx $id #x10000))

    ((define-reg-16 $id $id-h $id-l)
      (define-rules-syntax
        (($id) (u16-88 ($id-h) ($id-l)))
        (($id $u16)
          (let (($var $u16))
            ($id-h (u16-h $var))
            ($id-l (u16-l $var))))))

    ((define-mux-8 $id $idx $arg ...)
      (begin
        (define-mem mem (idx-size $idx))
        (define-rules-syntax
          (($id) (mem ($idx)))
          (($id $expr) (mem ($idx) $expr))))))
)
