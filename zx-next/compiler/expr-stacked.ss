(library (zx-next compiler expr-stacked)
  (export
    expr->stacked
    check-expr->stacked)
  (import (micascheme))

  (define (expr->stacked $indexed)
    (syntax-case $indexed ()
      ((size preserves-regs? op (arg-size . x) ...)
        (lets
          ($stacked-args (reverse (map expr->stacked #'((arg-size . x)...))))
          #`(
            #,@(flatten $stacked-args)
            (arg-size ... size preserves-regs? op))))))

  (define-rule-syntax (check-expr->stacked in out)
    (check (equal? (syntax->datum (expr->stacked #'in)) 'out)))
)
