(library (leo)
  (export leo)
  (import
    (except (micascheme) function)
    (term)
    (type)
    (typed)
    (compiler))

  (define-syntax leo
    (lambda (stx)
      (syntax-case stx ()
        ((leo expr)
          (lets 
            ($expr #`expr)
            ($typed-term (leo-compile $expr))
            ($term (typed-value $typed-term))
            (term->syntax $term))))))
)
