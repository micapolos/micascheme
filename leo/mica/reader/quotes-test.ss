(import
  (prefix (micascheme) %)
  (only (micascheme) quote)
  (mica reader)
  (leo mica reader quotes))

(check-reader begin-quote
  (ok "'" 'quasiquote)
  (error "`")
  (error ",")
  (error "''"))

(check-reader end-quote
  (ok "'" 'unquote)
  (ok "'." 'unquote-splicing)
  (error ",")
  (error "`")
  (error "``"))
