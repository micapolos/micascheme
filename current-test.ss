(import (micascheme) (current))

(let ()
  (do (define $value 0))
  (do (check (equal? (unsafe-current-get (unsafe-current $value)) 0)))
  (do (set! $value 1))
  (do (check (equal? (unsafe-current-get (unsafe-current $value)) 1)))
  (void))

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

(lets
  ((cons $randoms-a $randoms-b)
    (unsafe-current-get
      (lets
        (in current
          ($random-seed (current-random-seed))
          ($random1a (current-random))
          ($random2a (current-random))
          ($random3a (current-random))
          (do (set-current-random-seed $random-seed))
          ($random1b (current-random))
          ($random2b (current-random))
          ($random3b (current-random))
          (current
            (cons
              (list $random1a $random2a $random3a)
              (list $random1b $random2b $random3b)))))))
  ((cons $first-random $other-randoms) $randoms-a)
  (do (check (equal? $randoms-a $randoms-b)))
  (do
    (check
      (for-all
        (lambda ($other-random) (not (= $other-random $first-random)))
        $other-randoms)))
  (void))
