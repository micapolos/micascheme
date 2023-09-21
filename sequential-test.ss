(import (micascheme) (sequential) (sequential-lib))

(sequential
  zero
  counter
  (osc 0.25)
  (variable 0 (= (osc 0.25) 0) counter))
