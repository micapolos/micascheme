(library (minic lang)
  (export minic)
  (import
    (micascheme)
    (minic runtime)
    (minic syntax-type)
    (prefix (emu math) emu-))
  (export (import (minic runtime)))

  (define-case-syntax (minic body)
    (run
      (syntax->type #'body)
      #'body))
)
