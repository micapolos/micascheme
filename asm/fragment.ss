(library (asm fragment)
  (export
    fragment fragment? fragment-parameters fragment-block
    fragment->datum
    syntax->fragment)
  (import (micascheme) (syntax lookup) (asm block) (asm expression))

  (data (fragment parameters block))

  (define (fragment->datum $fragment)
    `(fragment
      (,@(map syntax->datum (fragment-parameters $fragment)))
      ,(block->datum (fragment-block $fragment))))

  (define (syntax->fragment $syntax)
    (syntax-case $syntax (begin db dw call ret)
      ((db arg ...)
        (lets
          ($expressions (map (partial syntax->expression '()) #'(arg ...)))
          (fragment
            (apply append (map expression-parameters $expressions))
            (block
              (length $expressions)
              (lambda ($port-identifier)
                (map-with ($expression (reverse $expressions))
                  #`(put-u8 #,$port-identifier
                    #,(expression-syntax $expression))))))))))
)
