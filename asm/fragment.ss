(library (asm fragment)
  (export
    fragment fragment? fragment-parameters fragment-block
    fragment-with
    fragment-append
    fragment->datum
    syntax->fragment)
  (import (micascheme) (syntax lookup) (asm block) (asm expression))

  (data (fragment parameters block))

  (define-rule-syntax (fragment-with (parameter ...) block)
    (fragment (stack #'parameter ...) block))

  (define (parameters-push $parameters $parameter)
    (if (exists (partial free-identifier=? $parameter) $parameters)
      $parameters
      (push $parameters $parameter)))

  (define (parameters-append . $parameters-list)
    (fold-left
      (lambda ($folded $parameters)
        (fold-left parameters-push $folded $parameters))
      (stack)
      $parameters-list))

  (define (fragment-append . $fragments)
    (fragment
      (apply parameters-append (map fragment-parameters $fragments))
      (apply block-append (map fragment-block $fragments))))

  (define (fragment->datum $fragment)
    `(fragment
      (,@(map syntax->datum (reverse (fragment-parameters $fragment))))
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
