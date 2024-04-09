(import (scheme) (check) (variable) (current) (lets))

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
        ((current $var1) (current-variable "foo"))
        ((current $var2) (current-variable "bar"))
        ((current $value1) (get-current $var1))
        ((current $value2) (get-current $var2))
        ((current _) (set-current $var1 (string-append $value1 "+")))
        ((current $value1) (get-current $var1))
        ((current _) (set-current $var2 (string-append $value1 $value2)))
        (get-current $var2)))
    "foo+bar"))
