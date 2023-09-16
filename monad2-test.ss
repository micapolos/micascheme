(import (micascheme) (monad2))

(check
  (equal?
    (monad-lets opt-monad 128)
    128))

(check
  (equal?
    (monad-lets opt-monad (do 128))
    128))

(check
  (equal?
    (monad-lets identity-monad
      ($box (box ""))
      ($x "foo")
      (do (set-box! $box $x))
      ($y (string-append "unboxed: " (unbox $box)))
      (do $y))
    "unboxed: foo"))

(check
  (equal?
    (monad-lets opt-monad
      ($x #f)
      ($y "world!")
      (string-append $x $y))
    #f))

(check
  (equal?
    (monad-lets opt-monad
      ($x "Hello, ")
      ($y #f)
      (string-append $x $y))
    #f))

(check
  (equal?
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
  (do (check (equal? (timed-get $timed 1) 2)))
  (do (check (equal? (timed-get $timed 2) 3)))
  (void))
