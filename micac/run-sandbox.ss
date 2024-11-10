(import (micascheme) (micac run))

(micac-run
  (extern printf)
  (var int x)
  (set x 2)
  (printf "First value: %i\\n" x)
  (set+ x 3)
  (set-bitwise-arithmetic-shift-left x 2)
  (printf "Second value: %i\\n" x)
  (while x
    (printf "Loop: %i\\n" x)
    (set- x 1))
  (printf "Done: %i\\n" (+ x 100)))
