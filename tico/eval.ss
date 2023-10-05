(library (tico eval)
  (export bindings-eval)
  (import (micascheme))

  (define bindings-parameter
    (make-thread-parameter (stack)))

  (define evaluate-environment
    (lets
      ($environment (copy-environment (environment `(micascheme))))
      (do (define-top-level-value `bindings-parameter bindings-parameter $environment))
      $environment))

  (define (bindings-eval $bindings $datum)
    (lets
      ($datum
        `(let-values
          ((
            (,@(map car $bindings))
            (apply values (map cdr (bindings-parameter)))))
          ,$datum))
      (parameterize ((bindings-parameter $bindings))
        (eval $datum evaluate-environment))))
)
