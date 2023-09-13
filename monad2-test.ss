(import (micascheme) (monad2))

(check
  (obj=?
    (monad-lets opt-monad 128)
    128))

(check
  (obj=?
    (monad-lets opt-monad
      ($x "Hello, ")
      ($y "world!")
      (string-append $x $y))
    "Hello, world!"))

(check
  (obj=?
    (monad-lets opt-monad
      ($x #f)
      ($y "world!")
      (string-append $x $y))
    #f))

(check
  (obj=?
    (monad-lets opt-monad
      ($x "Hello, ")
      ($y #f)
      (string-append $x $y))
    #f))

(check
  (obj=?
    (monad-lets opt-monad
      ($x #f)
      ($y #f)
      (string-append $x $y))
    #f))

; --------------------------

(lets
  ($timed
    (monad-lets timing
      ($time time-timed)
      (+ $time 1)))
  (do (check (obj=? (timed-get $timed 1) 2)))
  (do (check (obj=? (timed-get $timed 2) 3)))
  (void))
