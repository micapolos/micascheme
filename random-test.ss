(import (check) (random) (lets))

(lets
  ($randoms-pair
    (unsafe-current-get
      (lets
        ((current $random-seed) current-random-seed)
        ((current $random1a) current-random)
        ((current $random2a) current-random)
        ((current $random3a) current-random)
        ((current _) (set-current-random-seed $random-seed))
        ((current $random1b) current-random)
        ((current $random2b) current-random)
        ((current $random3b) current-random)
        (current
          (cons
            (list $random1a $random2a $random3a)
            (list $random1b $random2b $random3b))))))
  ($randoms-a (car $randoms-pair))
  ($randoms-b (cdr $randoms-pair))
  ($first-random (car $randoms-a))
  ($other-randoms (cdr $randoms-a))
  (let ()
    (check (equal? $randoms-a $randoms-b))
    (check
      (for-all
        (lambda ($other-random) (not (= $other-random $first-random)))
        $other-randoms))))
