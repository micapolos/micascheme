(import
  (prefix (micascheme) %)
  (mica parser)
  (leo mica parser))

(check-parses identifier-string "foo" "foo")
(check-parses identifier-string "variable-1" "variable-1")
(check-parses identifier-string "+" "+")
(check-parses identifier-string "-" "-")
(check-parses identifier-string "..." "...")
(check-parses identifier-string "->" "->")
(check-parses identifier-string "->foo" "->foo")

(check-parse-error identifier-string ":")
(check-parse-error identifier-string ":foo")
(check-parse-error identifier-string "foo:bar")
(check-parse-error identifier-string "foo:")

(check-parses special-atom "#t" #t)
(check-parses special-atom "#f" #f)
(check-parses special-atom "#\\a" #\a)
(check-parses special-atom "#\\space" #\space)
