(library (emu reg)
  (export
    define-reg
    define-r8
    define-r16)
  (import
    (scheme)
    (syntax)
    (syntaxes)
    (emu u))

  (define-rule-syntax (define-reg $reg)
    (begin
      (define $var 0)
      (define-rules-syntax ()
        (($reg) $var)
        (($reg $expr) (set! $var $expr)))))

  (define-rule-syntax (define-r8 $r8)
    (define-reg $r8))

  (define-rules-syntax ()
    ((define-r16 $r16)
      (define-reg $r16))
    ((define-r16 $r16 $r8-h $r8-l)
      (begin
        (define-r8 $r8h)
        (define-r8 $r8l)
        (define-rules-syntax ()
          (($r16) (u16-88 ($r8-h) ($r8-l)))
          (($r16 $u16)
            (let (($u16-id $u16))
              ($r8-h (u16-h $u16-id))
              ($r8-l (u16-l $u16-id))))))))
)
