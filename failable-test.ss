(import (scheme) (check) (failure) (failable) (lets) (procedure))


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
    (lets
      ((failable-failure $value) 128)
      (failure `(fatal ,$value)))
    128))

(check
  (equal?
    (lets
      ((failable-failure $value) (failure 'error))
      (failure `(fatal ,$value)))
    (failure '(fatal error))))
