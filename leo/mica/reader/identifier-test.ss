(import
  (prefix (micascheme) %)
  (only (micascheme) quote)
  (mica reader)
  (leo mica reader identifier))

(check-reader identifier
  (ok "foo" 'foo)
  (ok "variable-1" 'variable-1)
  (ok "+" '+)
  (ok "-" '-)
  (ok "..." '...)
  (ok "->" '->)
  (ok "->foo" '->foo)
  (error ":")
  (error ":foo")
  (error "foo:bar")
  (error "foo:"))
