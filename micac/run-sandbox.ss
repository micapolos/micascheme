(import (micascheme) (micac run))

(micac-run
  (var u8 x)
  (set x 2)
  (printf "First value: %i\\n" x)
  (add x 3)
  (shl x 2)
  (printf "Second value: %i\\n" x)
  (while x
    (printf "Loop: %i\\n" x)
    (sub x 1))
  (printf "Done: %i\\n" (+ x 100)))
