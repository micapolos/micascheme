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
    (emu mem))

  (define-rules-syntaxes
    ((define-idx $idx $size)
      (begin
        (define $box (box 0))
        (define-rules-syntax
          (($idx) (unbox $box))
          (($idx $expr) (set-box! $box $expr)))
        (define-property $idx idx-size $size)))

    ((define-reg-8 $reg-8)
      (define-idx $reg-8 #x100))

    ((define-reg-16 $reg-16)
      (define-idx $reg-16 #x10000))

    ((define-reg-16 $reg-16 $reg-8h $reg-8l)
      (define-rules-syntax
        (($reg-16) (u16-88 ($reg-8h) ($reg-8l)))
        (($reg-16 $u16)
          (let (($u16-id $u16))
            ($reg-8h (u16-h $u16-id))
            ($reg-8l (u16-l $u16-id))))))

    ((define-mux-8 $mux-8 $idx $arg ...)
      (begin
        (define-mem mem (idx-size $idx))
        (define-rules-syntax
          (($mux-8) (mem ($idx)))
          (($mux-8 $expr) (mem ($idx) $expr))))))

  (define-lookup-syntax (idx-size $syntax $lookup)
    (syntax-case $syntax ()
      (($id $idx)
        (lets
          ($size ($lookup #'$idx #'idx-size))
          (run (unless $size (syntax-error #'$idx "not idx:")))
          (datum->syntax #'$id $size)))))
)
