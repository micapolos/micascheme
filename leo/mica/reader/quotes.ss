(library (leo mica reader quotes)
  (export begin-quote end-quote)
  (import
    (prefix (micascheme) %)
    (only (micascheme) define quote)
    (mica reader))

  (define begin-quote
    (one-of
      (replace "'" 'quote)
      (replace "`" 'quasiquote)))

  (define end-quote
    (prefixed "`"
      (or
        (optional (replace "..." 'unquote-splicing))
        (return 'unquote))))
)
