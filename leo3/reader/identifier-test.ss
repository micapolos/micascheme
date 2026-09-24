(import
  (prefix (micascheme) %)
  (only (micascheme) quote)
  (mica reader)
  (leo3 reader identifier))

(check-reader letter-char
  (ok "a" #\a)
  (ok "z" #\z)
  (error "A")
  (error "Z")
  (error "1"))

(check-reader word-string
  (ok "foo" "foo")
  (error ""))

(check-reader identifier
  (ok "foo" 'foo)
  (ok "foo bar" 'foo-bar)
  (error ""))
