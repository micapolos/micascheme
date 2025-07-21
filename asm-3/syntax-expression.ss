(library (asm-3 syntax-expression)
  (export syntax->expression)
  (import (asm-3 base) (asm-3 org) (asm-3 expression))

  (define (syntax->expression $syntax)
    (syntax-case $syntax (org)
      (org
        (org-expression))
      (id
        (identifier? #'id)
        (identifier-expression #'id))
      (literal
        ((or? boolean? integer? string? char?) (datum literal))
        (pure-expression (datum literal)))
      ((fn arg ...)
        (apply application-expression
          (syntax->expression #'fn)
          (map syntax->expression #'(arg ...))))
      (other
        (syntax-error #'other "not expression"))))
)
