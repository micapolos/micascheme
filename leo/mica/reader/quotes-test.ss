(import
  (prefix (micascheme) %)
  (mica reader)
  (leo mica reader quotes))

(check-reader quote
  (ok "'" (%quote quote))
  (ok "`" (%quote quasiquote))
  (error ",")
  (error "''"))

(check-reader unquote
  (ok "`" (%quote unquote))
  (ok "`..." (%quote unquote-splicing))
  (error ",")
  (error "``"))
