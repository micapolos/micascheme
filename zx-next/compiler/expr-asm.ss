(library (zx-next compiler expr-asm)
  (export
    expr->asm
    check-expr->asm)
  (import
    (micascheme)
    (zx-next compiler expr-stacked)
    (zx-next compiler stacked-asm))

  (define (expr->asm $expr)
    (syntax-case $expr ()
      ((regs x)
        (stacked->asm #`(regs . #,(expr->stacked #'x))))))

  (define-rule-syntax (check-expr->asm in out)
    (check (equal? (syntax->datum (expr->asm #'in)) 'out)))
)
