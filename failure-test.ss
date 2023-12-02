(import (check) (failure) (lets))

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

(check
  (equal?
    (failable-with 'error-1
      (failable-with 'error-2
        (failure 'origin)))
    (failure 'error-1 'error-2 'origin)))
