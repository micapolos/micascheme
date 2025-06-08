(library (asm parameters)
  (export
    empty-parameters
    parameters-push
    parameters-append)
  (import (micascheme))

  (define (empty-parameters) (stack))

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
)
