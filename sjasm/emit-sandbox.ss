(import (micascheme) (sjasm emit))

(emit "Hello, world!")
(emit "\n")
(display (emitted))
(save "~/Desktop/emitted.txt")
