(library (tt procedure)
  (export partial)
  (import
    (tt lang)
    (prefix (tt lang-macros) %)
    (prefix (scheme) %))

  (define-macro partial %compile-partial)
)
