(library (zx-next compiler expr-stacked)
  (export
    expr->stacked
    check-expr->stacked)
  (import (micascheme))

  (define (expr->stacked $indexed)
    (syntax-case $indexed ()
      ((1 op)
        #'(1 op))
      ((1 op (1 a) (1 b))
        #`(
          #,(expr->stacked #'(1 b))
          #,(expr->stacked #'(1 a))
          (1 1 1 op)))))

  (define-rule-syntax (check-expr->stacked in out)
    (check (equal? (syntax->datum (expr->stacked #'in)) 'out)))
)
