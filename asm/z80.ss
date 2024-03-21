(library (asm z80)
  (export org db dw)
  (import
    (micascheme)
    (asm core))

  (define (emit-dw $emit $u16)
    (run
      ($emit (fxand $u16 #xff))
      ($emit (fxsrl $u16 8))))

  (define-asm-syntax org
    (lambda ($syntax $emit $org)
      (syntax-case $syntax ()
        ((_ $value)
          (and (integer? (datum $value)) (nonnegative? (datum $value)))
          (run
            ($org (datum $value))
            #`(begin))))))

  (define-asm-syntax db
    (lambda ($syntax $emit $org)
      (syntax-case $syntax ()
        ((_ $expr ...)
          (run
            ($org (+ ($org) (length (syntax->list #'($expr ...)))))
            #`(begin
              (#,$emit $expr) ...))))))

  (define-asm-syntax dw
    (lambda ($syntax $emit $org)
      (syntax-case $syntax ()
        ((_ $expr ...)
          (run
            ($org (+ ($org) (* 2 (length (syntax->list #'($expr ...))))))
            #`(begin (emit-dw #,$emit $expr) ...))))))
)
