(import (micascheme) (micac run))

(parameterize ((micac-run-echo? #t))
  (micac-run
    (extern printf)
    (printf "do\\n")))
