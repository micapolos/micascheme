(library
  (leo ftype-ref)
  (export ftype-ref ftype*-ref)
  (import
    (rename (scheme) (ftype-ref %ftype-ref))
    (syntaxes))

  (define-rules-syntaxes
    ((ftype-ref ftype-name x a ...)
      (%ftype-ref ftype-name (a ...) x))
    ((ftype*-ref ftype-name a ... x index)
      (%ftype-ref ftype-name x (a ...) index)))
)
