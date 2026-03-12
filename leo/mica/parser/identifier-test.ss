(import
  (prefix (micascheme) %)
  (only (micascheme) quote)
  (mica parser)
  (leo mica parser identifier))

(check-parser identifier
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
