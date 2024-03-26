(library (from)
  (export from)
  (import
    (scheme)
    (syntax))

  (define-rule-syntax (from $package-spec $identifier)
    (let ()
      (import-only (only $package-spec $identifier))
      $identifier))
)
