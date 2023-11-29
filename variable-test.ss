(import (check) (variable) (current) (lets))

(let ()
  (define $value 0)
  (check (equal? (unsafe-current-get (unsafe-current $value)) 0))
  (set! $value 1)
  (check (equal? (unsafe-current-get (unsafe-current $value)) 1)))

(check
  (equal?
    (unsafe-current-get (current 10))
    10))

(check
  (equal?
    (unsafe-current-get
      (lets
        (in current
          ($var1 (current-variable "foo"))
          ($var2 (current-variable "bar"))
          ($value1 (get-current $var1))
          ($value2 (get-current $var2))
          (do (set-current $var1 (string-append $value1 "+")))
          ($value1 (get-current $var1))
          (do (set-current $var2 (string-append $value1 $value2)))
          (get-current $var2))))
    "foo+bar"))
