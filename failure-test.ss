(import (check) (failure))

; === failable-let ===

(check
  (equal?
    (failable-let ($number 128) (+ $number 1))
    129))

(check
  (equal?
    (failable-let ($number 128) (failure `dupa))
    (failure `dupa)))

(check
  (equal?
    (failable-let ($number (failure `dupa)) (+ $number 1))
    (failure `dupa)))
