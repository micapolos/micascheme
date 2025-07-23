(library (asm-3 syntax-expression)
  (export syntax->expression)
  (import (asm-3 base) (asm-3 org) (asm-3 expression))

  (define (syntax->expression $lookup $syntax)
    (syntax-case $syntax (org)
      (org
        (org-expression))
      (id
        (identifier? #'id)
        (identifier-expression #'id))
      (literal
        ((or? boolean? integer? string? char?) (datum literal))
        (pure-expression (datum literal)))
      ((id . x)
        (and (identifier? #'id) ($lookup #'id))
        (switch (($lookup #'id) $lookup $syntax)
          ((syntax? $syntax) (syntax->expression $lookup $syntax))
          ((else $expression) $expression)))
      ((fn arg ...)
        (apply application-expression
          (syntax->expression $lookup #'fn)
          (map (partial syntax->expression $lookup) #'(arg ...))))
      (other
        (syntax-error #'other "not expression"))))
)
