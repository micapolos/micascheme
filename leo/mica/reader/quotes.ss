(library (leo mica reader quotes)
  (export quote unquote)
  (import
    (prefix (micascheme) %)
    (only (micascheme) define)
    (mica reader))

  (define quote
    (one-of
      (replace "'" (%quote quote))
      (replace "`" (%quote quasiquote))))

  (define unquote
    (prefixed "`"
      (or
        (optional (replace "..." (%quote unquote-splicing)))
        (return (%quote unquote)))))
)
