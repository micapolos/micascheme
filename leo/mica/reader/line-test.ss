(import
  (mica reader)
  (only (micascheme) quote)
  (leo mica reader line))

(check-reader line
  (ok "10\n" 10)
  (ok "foo\n" 'foo)
  (ok "foo bar\n" '(foo bar))

  (error "")
  (error "10")
  (error "foo"))
