(library (tata-8 typed)
  (export
    typed
    typed?
    typed-type
    typed-ref)
  (import
    (scheme)
    (data)
    (union))

  (data (typed type ref))
)
