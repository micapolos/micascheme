(library (leo3 environment)
  (export
    scheme-interaction-environment
    leo-interaction-environment)
  (import
    (scheme)
    (syntax))

  (define scheme-interaction-environment
    (interaction-environment))

  (define leo-interaction-environment
    (copy-environment (environment '(leo3 scheme))))
)
