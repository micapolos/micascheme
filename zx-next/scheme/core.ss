(library (zx-next scheme core)
  (export
    scheme-throw)
  (import
    (zx-next core)
    (zx-next scheme value))

  (define-op (scheme-throw)
    (input (e error-code) (hla value))
    (break))
)
