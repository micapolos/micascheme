(import
  (prefix (micascheme) %)
  (only (micascheme) quote)
  (mica reader)
  (leo3 reader literal))

(check-reader literal
  (ok "123" 123)
  (ok "\"123\"" "123")
  (error "foo"))

