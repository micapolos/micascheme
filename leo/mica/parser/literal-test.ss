(import
  (prefix (micascheme) %)
  (only (micascheme) quote)
  (mica parser)
  (leo mica parser literal))

(check-parses number "0" 0)
(check-parses number "1" 1)
(check-parses number "123" 123)
(check-parse-error number "")
(check-parse-error number "-1") ; TODO: negative numbers
(check-parse-error number "+1") ; TODO: positive numbers
(check-parse-error number "3.14") ; TODO: float numbers

(check-parses string-literal "\"\"" "")
(check-parses string-literal "\"foo\"" "foo")
(check-parses string-literal "\"foo bar\"" "foo bar")

(check-parse-error string-literal "\"\n\"") ; TODO: escaping

(check-parses special-literal "#t" #t)
(check-parses special-literal "#f" #f)
(check-parses special-literal "#\\a" #\a)
(check-parses special-literal "#\\space" #\space)

(check-parses literal "123" 123)
(check-parses literal "foo" 'foo)
(check-parses literal "#t" #t)
(check-parses literal "#\\space" #\space)
(check-parse-error literal "(foo)")
