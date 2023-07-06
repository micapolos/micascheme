(library (leo)
  (export leo)
  (import (micascheme) (term) (type) (typed))

  (define-syntax leo
    (lambda (stx)
      (syntax-case stx ()
        ((leo expr)
          (let* (($expr #`expr)
                 ($typed-term (parse (list) $expr))
                 ($term (typed-value $typed-term))
                 ($datum (term->datum $term)))
            (datum->syntax #`leo $datum))))))
)
