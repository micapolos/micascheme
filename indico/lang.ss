(library (indico lang)
  (export indico)
  (import
    (scheme)
    (syntax)
    (list)
    (stack)
    (indico keywords)
    (indico expr))

  (export (import (indico keywords)))

  (define-case-syntax (indico body)
    #`(values->list
      #,(expr-syntax (syntax->expr (stack) #'body))))
)
