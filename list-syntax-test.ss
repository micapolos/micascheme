(import (scheme) (check) (list-syntax))

(check
  (equal?
    (map-with
      (number (list 1 2 3))
      (string (list "one" "two" "three"))
      (string-append (number->string number) ": " string))
    (list
      "1: one"
      "2: two"
      "3: three")))

(let ()
  (define $string "Hello")
  (for ($x (list "world" "universe"))
    (set! $string (string-append $string ", "))
    (set! $string (string-append $string $x)))
  (set! $string (string-append $string "!"))
  (check (equal? $string "Hello, world, universe!")))

