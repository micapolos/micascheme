(library (leo3 environment)
  (export leo-interaction-environment)
  (import (scheme))

  (define leo-interaction-environment
    (make-parameter (copy-environment (environment '(leo3 scheme)))))
)
