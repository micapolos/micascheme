(library (simplang lang)
  (export simplang)
  (import (micascheme) (simplang expander))

  (define-case-syntax (simplang expr)
    #`(eval
      '#,(datum->syntax #'+ (cdr (typed '() (or (syntax->annotation #'expr) (syntax->datum #'expr)))))
      (environment '(scheme))))
)
