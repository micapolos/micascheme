(import
  (prefix (micascheme) %)
  (mica parser)
  (leo mica parser literal))

(check-parses literal "#t" #t)
(check-parses literal "#f" #f)
(check-parses literal "#\\a" #\a)
(check-parses literal "#\\space" #\space)
