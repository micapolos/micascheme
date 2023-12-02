(import (check) (failure) (lets))

; === failable-let ===

(check
  (equal?
    (lets
      ((failable $number) 128)
      (+ $number 1))
    129))

(check
  (equal?
    (lets
      ((failable $number) 128)
      (failure 'dupa))
    (failure `dupa)))

(check
  (equal?
    (lets
      ((failable $number) (failure 'dupa))
      (+ $number 1))
    (failure 'dupa)))
