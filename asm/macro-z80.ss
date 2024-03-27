(library (asm macro-z80)
  (export
    db db-233 dw

    b c d e h l a
    ixh ixl iyh iyl
    bc de hl af
    ix iy
    pc sp
    i r
    p q)
  (import
    (micascheme)
    (labs macro))

  (define (db $u8)
    (writeln `(db ,$u8)))

  (define (db-233 $a $b $c)
    (db (fxior (fxsll $a 6) (fxsll $b 3) $c)))

  (define (dw $u16)
    (run-void
      (db (fxand $u16 #xff))
      (db (fxsrl $u16 8))))

  (define-rule-syntax (define-literals $r ...)
    (begin
      (define-aux-keyword $r) ...
      (define-syntax-literal? $r) ...))

  (define-literals b c d e h l a)
  (define-literals ixh ixl iyh iyl)

  (define-literals bc de hl af)
  (define-literals ix iy)

  (define-literals pc sp)
  (define-literals i r)

  (define-literals nz z nc po pe m)

  (define-syntax-literal? +)
  (define-syntax-literal? -)

  (define-aux-keywords p q)

)
