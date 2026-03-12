(import
  (prefix (micascheme) %)
  (mica parser)
  (leo mica parser literal))

(check-parses literal "#t" #t)
(check-parses literal "#f" #f)
(check-parses literal "#\\a" #\a)
(check-parses literal "#\\space" #\space)

(check-parses number "0" 0)
(check-parses number "1" 1)
(check-parses number "123" 123)
(check-parse-error number "")
(check-parse-error number "-1") ; TODO: negative numbers
(check-parse-error number "+1") ; TODO: positive numbers
(check-parse-error number "3.14") ; TODO: float numbers
