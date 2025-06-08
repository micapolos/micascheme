(library (asm expression)
  (export
    expression expression? expression-parameters expression-syntax
    expression->datum
    syntax->expression
    u8? u16?)
  (import (micascheme))

  (data (expression parameters syntax))

  (define (u8? $obj)
    (and
      (integer? $obj)
      (>= $obj #x00)
      (<= $obj #xff)))

  (define (u16? $obj)
    (and
      (integer? $obj)
      (>= $obj #x0000)
      (<= $obj #xffff)))

  (define (expression->datum $expression)
    `(expression
      (,@(map syntax->datum (expression-parameters $expression)))
      ,(syntax->datum (expression-syntax $expression))))

  (define (syntax->expression $locals $syntax)
    (syntax-case $syntax (+)
      (id
        (identifier? #'id)
        (cond
          ((exists (partial free-identifier=? #'id) $locals)
            (expression (stack) #'id))
          (else
            (expression (stack #'id) #'id))))
      (literal
        (literal? (datum literal))
        (expression (stack) #'literal))
      ((+ arg ...)
        (lets
          ($expressions (map (partial syntax->expression $locals) #'(arg ...)))
          (expression
            (apply append (map expression-parameters $expressions))
            #`(+ #,@(map expression-syntax $expressions)))))))
)
