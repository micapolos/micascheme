(import
  (prefix (micascheme) %)
  (only (micascheme) quote)
  (mica reader)
  (leo mica reader quoted))

(check-reader
  (map
    (begin-quoted-annotation (annotation numeric-char))
    %annotation-stripped)
  (ok "1" #\1)
  (ok "'1" ''#\1)
  (ok "''1" '''#\1)
  (ok "`1" '`#\1)
  (ok "``1" '``#\1))
