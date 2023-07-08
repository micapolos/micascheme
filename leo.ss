(library (leo)
  (export leo)
  (import (micascheme) (variable) (term) (type) (parser))

  (define-syntax leo
    (lambda (stx)
      (syntax-case stx ()
        ((leo expr)
          (lets 
            ($expr #`expr)
            ($typed-term (parse $expr))
            ($term (typed-value $typed-term))
            ($datum (term->datum $term))
            (datum->syntax #`leo $datum))))))
)
