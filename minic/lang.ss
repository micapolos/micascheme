(library (minic lang)
  (export minic)
  (import (micascheme) (minic keyword) (minic syntax))
  (export (import (minic keyword)))

  (define-case-syntax (minic body)
    (parse #'body))
)
