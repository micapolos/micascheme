(library (simplang lang)
  (export simplang)
  (import (micascheme) (simplang expander) (simplang core))

  (define-case-syntax (simplang expr)
    #`(eval
      '#,(datum->syntax #'+ (cdr (typed core-scope (syntax->datum/annotation #'expr))))
      (environment '(scheme))))
)
