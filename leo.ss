(library (leo)
  (export leo)
  (import (micascheme) (variable) (term) (type) (compiler))

  (define-syntax leo
    (lambda (stx)
      (syntax-case stx ()
        ((leo expr)
          (lets 
            ($expr #`expr)
            ($typed-term (parse $expr))
            ($term (typed-value $typed-term))
            (term->syntax $term))))))
)
