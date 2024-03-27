(library (asm macro-z80)
  (export
    define-literals
    db db-233 dw)
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

  (define-syntax-literal? +)
  (define-syntax-literal? -)
)
