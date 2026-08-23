(import
  (only (scheme) quote)
  (mica reader)
  (leo3 reader))

(check-reader word
  (ok "foo" "foo")
  (error "")
  (error "1")
  (error "a1"))

(check-reader identifier
  (ok "foo" 'foo)
  (ok "foo bar" 'foo-bar)
  (ok "foo to bar" 'foo-bar)
  (error "")
  (error "1")
  (error "a1"))
