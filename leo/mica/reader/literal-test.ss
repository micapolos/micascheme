(import
  (prefix (micascheme) %)
  (only (micascheme) quote)
  (mica reader)
  (leo mica reader literal))

(check-reader number
  (ok "0" 0)
  (ok "1" 1)
  (ok "123" 123)
  (error "")
  (error "-1") ; TODO: negative numbers
  (error "+1") ; TODO: positive numbers
  (error "3.14")) ; TODO: float numbers

(check-reader string-literal
  (ok "\"\"" "")
  (ok "\"foo\"" "foo")
  (ok "\"foo bar\"" "foo bar")
  (error "\"\n\"")) ; TODO: escaping

(check-reader special-literal
  (ok "#t" #t)
  (ok "#f" #f)
  (ok "#\\a" #\a)
  (ok "#\\space" #\space))

(check-reader literal
  (ok "123" 123)
  (ok "foo" 'foo)
  (ok "#t" #t)
  (ok "#\\space" #\space)
  (error "(foo)"))
