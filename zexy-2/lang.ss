(library (zexy-2 lang)
  (export
    define-fragment)
  (import
    (micascheme)
    (zexy-2 fragment))

  (define-rule-syntax (define-fragment name value)
    (define-syntax name
      (make-compile-time-value value)))
)
