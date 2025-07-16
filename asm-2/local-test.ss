(import (micascheme) (asm-2 local) (asm-2 fragment))

(check
  (works
    (local-with (a 10) (b 20)
      (fragment-with (dep a b) (+ (dep a) (dep b))))))
