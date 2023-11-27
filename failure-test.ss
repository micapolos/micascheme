(import (check) (failure))

; === fallible-let ===

(check
  (equal?
    (fallible-let ($number 128) (+ $number 1))
    129))

(check
  (equal?
    (fallible-let ($number 128) (failure `dupa))
    (failure `dupa)))

(check
  (equal?
    (fallible-let ($number (failure `dupa)) (+ $number 1))
    (failure `dupa)))
