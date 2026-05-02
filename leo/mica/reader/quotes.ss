(library (leo mica reader quotes)
  (export begin-quote end-quote)
  (import
    (prefix (scheme) %)
    (only (scheme) quote)
    (mica reader))

  (%define begin-quote
    (one-of
      (replace "'" 'quasiquote)))

  (%define end-quote
    (prefixed "'"
      (or
        (optional (replace "." 'unquote-splicing))
        (return 'unquote))))
)
