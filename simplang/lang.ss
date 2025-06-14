(library (simplang lang)
  (export simplang)
  (import (micascheme) (simplang expander) (simplang core))

  (define-case-syntax (simplang expr)
    #`(eval
      '#,(datum->syntax #'+ (cdr (typed core-scope (or (syntax->annotation #'expr) (syntax->datum #'expr)))))
      (environment '(scheme))))
)
