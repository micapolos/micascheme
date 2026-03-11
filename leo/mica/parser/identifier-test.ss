(import
  (prefix (micascheme) %)
  (only (micascheme) quote)
  (mica parser)
  (leo mica parser identifier))

(check-parses identifier "foo" 'foo)
(check-parses identifier "variable-1" 'variable-1)
(check-parses identifier "+" '+)
(check-parses identifier "-" '-)
(check-parses identifier "..." '...)
(check-parses identifier "->" '->)
(check-parses identifier "->foo" '->foo)

(check-parse-error identifier ":")
(check-parse-error identifier ":foo")
(check-parse-error identifier "foo:bar")
(check-parse-error identifier "foo:")
