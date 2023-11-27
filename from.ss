(library (from)
  (export from)
  (import
    (scheme)
    (syntax))

  (define-syntax-rule (from $package-spec $identifier)
    (let ()
      (import-only (only $package-spec $identifier))
      $identifier))
)
