(import
  (prefix (micascheme) %)
  (only (micascheme) quote)
  (mica parser)
  (leo mica parser literal))

(check-parser number
  (ok "0" 0)
  (ok "1" 1)
  (ok "123" 123)
  (error "")
  (error "-1") ; TODO: negative numbers
  (error "+1") ; TODO: positive numbers
  (error "3.14")) ; TODO: float numbers

(check-parser string-literal
  (ok "\"\"" "")
  (ok "\"foo\"" "foo")
  (ok "\"foo bar\"" "foo bar")
  (error "\"\n\"")) ; TODO: escaping

(check-parser special-literal
  (ok "#t" #t)
  (ok "#f" #f)
  (ok "#\\a" #\a)
  (ok "#\\space" #\space))

(check-parser literal
  (ok "123" 123)
  (ok "foo" 'foo)
  (ok "#t" #t)
  (ok "#\\space" #\space)
  (error "(foo)"))
