(import (scheme) (check) (mica textual))

(check-textual #\a
  (ok "a")
  (error "b"))

(check-textual "foo"
  (ok "foo")
  (error "fo")
  (error "foob"))

(check-textual textual-null
  (ok "")
  (error "f"))

(check-textual textual-char
  (ok "f")
  (error "")
  (error "fo"))

(check-textual (textual-char-passing char-alphabetic?)
  (ok "f")
  (error "")
  (error "1")
  (error "fo"))
