(import (scheme) (check) (mica codec))

(check-codec char-codec
  (works "f" "1")
  (raises "" "fo"))

(check-codec (?char-codec char-alphabetic?)
  (works "f")
  (raises "1" "" "fo"))

(check-codec (?char-codec char-alphabetic?)
  (works "f")
  (raises "1" "" "fo"))
