(import (micascheme) (fib))

(displayln (time (fib 40)))
(displayln (time (fxfib 40)))
(displayln (time (fx3fib 40)))
(displayln (time (flfib 40.0)))
(displayln (time (flsinglefib 40.0)))
(displayln (time (fixfib 40)))
