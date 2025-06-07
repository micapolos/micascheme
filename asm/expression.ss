(library (asm expression)
  (export
    expression expression? expression-parameters expression-syntax
    syntax->expression)
  (import (micascheme))

  (data (expression parameters syntax))

  (define (syntax->expression $locals $syntax)
    (syntax-case $syntax (+)
      (literal
        (literal? (datum literal))
        (expression (stack) #'literal))
      (id
        (identifier? #'id)
        (cond
          ((exists (partial free-identifier=? #'id) $locals)
            (expression (stack) #'id))
          (else
            (expression (stack #'id) #'id))))
      ((+ arg ...)
        (lets
          ($expressions (map (partial syntax->expression $locals) #'(arg ...)))
          (expression
            (apply append (map expression-parameters $expressions))
            #`(+ #,@(map expression-syntax $expressions)))))))
)
