(import (scheme) (check) (list-syntax) (boolean))

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

(check (not-false? (for-all-with ($n1 (list 1 2 3)) ($n2 (list 1 2 3)) (= $n1 $n2))))
(check (false? (for-all-with ($n1 (list 1 2 3)) ($n2 (list 1 2 4)) (= $n1 $n2))))

(check (not-false? (exists-with ($n1 (list 1 2 3)) ($n2 (list 1 3 4)) (= $n1 $n2))))
(check (false? (exists-with ($n1 (list 1 2 3)) ($n2 (list 2 3 4)) (= $n1 $n2))))

(let ()
  (define $string "Hello")
  (for ($x (list "world" "universe"))
    (set! $string (string-append $string ", "))
    (set! $string (string-append $string $x)))
  (set! $string (string-append $string "!"))
  (check (equal? $string "Hello, world, universe!")))

