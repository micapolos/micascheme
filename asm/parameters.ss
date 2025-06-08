(library (asm parameters)
  (export
    parameters-with
    empty-parameters
    parameters-push
    parameters-append
    parameters->syntax)
  (import (micascheme))

  (define-rule-syntax (parameters-with parameter ...)
    (stack #'parameter ...))

  (define (empty-parameters)
    (parameters-with))

  (define (parameters-push $parameters $parameter)
    (if (exists (partial free-identifier=? $parameter) $parameters)
      $parameters
      (push $parameters $parameter)))

  (define (parameters-append . $parameters-list)
    (fold-left
      (lambda ($folded $parameters)
        (fold-left parameters-push $folded (reverse $parameters)))
      (stack)
      $parameters-list))

  (define (parameters->syntax $parameters)
    #`(#,@(reverse $parameters)))
)
